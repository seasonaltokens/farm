// Seasonal Token Farm

//SPDX-License-Identifier: MIT
pragma solidity 0.8.5;
pragma abicoder v2;

import "../interfaces/ERC20.sol";
import "../interfaces/ERC721TokenReceiver.sol";
import "../interfaces/ApproveAndCallFallBack.sol";


interface INonfungiblePositionManager {
    function positions(uint256 tokenId)
        external
        view
        returns (
            uint96 nonce,
            address operator,
            address token0,
            address token1,
            uint24 fee,
            int24 tickLower,
            int24 tickUpper,
            uint128 liquidity,
            uint256 feeGrowthInside0LastX128,
            uint256 feeGrowthInside1LastX128,
            uint128 tokensOwed0,
            uint128 tokensOwed1
        );
    function safeTransferFrom(address _from, address _to, uint256 _tokenId) external payable;
}


struct LiquidityToken {
    address owner;
    address seasonalToken;
    uint256 depositTime;
    uint256 initialCumulativeSpringTokensFarmed;
    uint256 initialCumulativeSummerTokensFarmed;
    uint256 initialCumulativeAutumnTokensFarmed;
    uint256 initialCumulativeWinterTokensFarmed;
    uint256 liquidity;
    uint256 position;
}

contract SeasonalTokenFarm is ERC721TokenReceiver, ApproveAndCallFallBack {

    // The Seasonal Token Farm runs on voluntary donations.

    // Incoming donated tokens are distributed to liquidity providers for the ETH/Token trading pairs.
    // Each trading pair has an allocationSize. Incoming tokens are allocated to trading pairs in
    // proportion to their allocationSizes. The fraction of tokens allocated to a trading pair is
    // equal to that trading pair's allocationSize divided by the sum of the allocationSizes.

    // The initial allocationSizes are 5, 6, 7 and 8 for Spring, Summer, Autumn and Winter.
    // A few months after each token's halving, the allocationSize for the ETH/Token trading pair
    // doubles. So after the first Spring halving, the allocationSizes will be 10, 6, 7 and 8.
    // After the first Summer halving, they will be 10, 12, 7 and 8.

    uint256 public constant REALLOCATION_INTERVAL = (365 * 24 * 60 * 60 * 3) / 4; // 9 months


    // Liquidity positions must cover the full range of prices

    int24 public constant MINIMUM_TICK_UPPER = 887200;
    int24 public constant MAXIMUM_TICK_LOWER = -887200;


    // Liquidity tokens can be withdrawn for 7 days out of every 37.
    //
    // This means that about one fifth of the liquidity can be withdrawn at any given time,
    // preventing liquidity from disappearing in a panic, but liquidity providers can withdraw
    // to adjust their positions monthly.

    uint256 public constant WITHDRAWAL_UNAVAILABLE_DAYS = 30;
    uint256 public constant WITHDRAWAL_AVAILABLE_DAYS = 7;


    // Each liquidity token deposited adds a specific amount of liquidity to the ETH/Seasonal Token
    // trading pair. Incoming tokens allocated to that trading pair are distributed to liquidity
    // token owners in proportion to the liquidity they have provided.

    mapping(address => uint256) public totalLiquidity;
    mapping(address => uint256[]) public tokenOfOwnerByIndex;
    mapping(uint256 => LiquidityToken) public liquidityTokens;

    address public immutable springTokenAddress;
    address public immutable summerTokenAddress;
    address public immutable autumnTokenAddress;
    address public immutable winterTokenAddress;
    address public immutable wethAddress;

    INonfungiblePositionManager public immutable nonfungiblePositionManager;

    uint256 public immutable startTime;
    

    // We keep track of the cumulative number of farmed (donated and allocated) tokens of each type per unit
    // liquidity, for each trading pair. This allows us to calculate the payout for each liquidity token.
    // 
    // When a liquidity token is deposited, the value of the cumulative number of farmed tokens per unit
    // liquidity is recorded. The number of tokens farmed by that liquidity position is given by the
    // amount of liquidity multiplied by the increase in the cumulative number of tokens farmed per
    // unit liquidity.
    //
    // cumulativeTokensFarmedPerUnitLiquidity[trading_pair_token][farmed_token] = farmed tokens/liquidity

    mapping(address => mapping(address => uint256)) public cumulativeTokensFarmedPerUnitLiquidity;

    event Deposit(address indexed from, uint256 liquidityTokenId);
    event Withdraw(address indexed tokenOwner, uint256 liquidityTokenId);
    event Donate(address indexed from, address seasonalTokenAddress, uint256 amount);
    event Harvest(address indexed tokenOwner, uint256 liquidityTokenId, 
                  uint256 springAmount, uint256 summerAmount, uint256 autumnAmount, uint256 winterAmount);


    constructor (INonfungiblePositionManager nonfungiblePositionManager_, 
                 address springTokenAddress_, 
                 address summerTokenAddress_,
                 address autumnTokenAddress_,
                 address winterTokenAddress_,
                 address wethAddress_,
                 uint256 startTime_) {

        nonfungiblePositionManager = nonfungiblePositionManager_;

        springTokenAddress = springTokenAddress_;
        summerTokenAddress = summerTokenAddress_;
        autumnTokenAddress = autumnTokenAddress_;
        winterTokenAddress = winterTokenAddress_;
        wethAddress = wethAddress_;

        startTime = startTime_;
    }

    function balanceOf(address liquidityProvider) external view returns (uint256) {
        return tokenOfOwnerByIndex[liquidityProvider].length;
    }

    function numberOfReallocations() internal view returns (uint256) {
        if (block.timestamp < startTime + REALLOCATION_INTERVAL)
            return 0;
        uint256 timeSinceStart = block.timestamp - startTime;
        return timeSinceStart / REALLOCATION_INTERVAL;
    }

    function hasDoubledAllocation(uint256 tokenNumber) internal view returns (uint256) {

        if (numberOfReallocations() % 4 < tokenNumber)
            return 0;
        
        return 1;
    }

    function springAllocationSize() public view returns (uint256) {
        return 5 * 2 ** hasDoubledAllocation(1);
    }

    function summerAllocationSize() public view returns (uint256) {
        return 6 * 2 ** hasDoubledAllocation(2);
    }

    function autumnAllocationSize() public view returns (uint256) {
        return 7 * 2 ** hasDoubledAllocation(3);
    }

    function winterAllocationSize() public pure returns (uint256) {
        return 8;
    }

    function getEffectiveTotalAllocationSize(uint256 totalSpringLiquidity, 
                                             uint256 totalSummerLiquidity,
                                             uint256 totalAutumnLiquidity,
                                             uint256 totalWinterLiquidity) internal view returns (uint256) {
        uint256 effectiveTotal = 0;

        if (totalSpringLiquidity > 0)
            effectiveTotal += springAllocationSize();
        if (totalSummerLiquidity > 0)
            effectiveTotal += summerAllocationSize();
        if (totalAutumnLiquidity > 0)
            effectiveTotal += autumnAllocationSize();
        if (totalWinterLiquidity > 0)
            effectiveTotal += winterAllocationSize();
        
        return effectiveTotal;
    }

    function allocateIncomingTokensToTradingPairs(address incomingTokenAddress, uint256 amount) internal {

        uint256 totalSpringLiquidity = totalLiquidity[springTokenAddress];
        uint256 totalSummerLiquidity = totalLiquidity[summerTokenAddress];
        uint256 totalAutumnLiquidity = totalLiquidity[autumnTokenAddress];
        uint256 totalWinterLiquidity = totalLiquidity[winterTokenAddress];

        uint256 effectiveTotalAllocationSize = getEffectiveTotalAllocationSize(totalSpringLiquidity,
                                                                               totalSummerLiquidity,
                                                                               totalAutumnLiquidity,
                                                                               totalWinterLiquidity);

        require(effectiveTotalAllocationSize > 0, "No liquidity in farm");

        uint256 springPairAllocation = (amount * springAllocationSize()) / effectiveTotalAllocationSize;
        uint256 summerPairAllocation = (amount * summerAllocationSize()) / effectiveTotalAllocationSize;
        uint256 autumnPairAllocation = (amount * autumnAllocationSize()) / effectiveTotalAllocationSize;
        uint256 winterPairAllocation = (amount * winterAllocationSize()) / effectiveTotalAllocationSize;

        if (totalSpringLiquidity > 0)
            cumulativeTokensFarmedPerUnitLiquidity[springTokenAddress][incomingTokenAddress] 
                += (2 ** 128) * springPairAllocation / totalSpringLiquidity;

        if (totalSummerLiquidity > 0)
            cumulativeTokensFarmedPerUnitLiquidity[summerTokenAddress][incomingTokenAddress] 
                += (2 ** 128) * summerPairAllocation / totalSummerLiquidity;

        if (totalAutumnLiquidity > 0)
            cumulativeTokensFarmedPerUnitLiquidity[autumnTokenAddress][incomingTokenAddress] 
                += (2 ** 128) * autumnPairAllocation / totalAutumnLiquidity;

        if (totalWinterLiquidity > 0)
            cumulativeTokensFarmedPerUnitLiquidity[winterTokenAddress][incomingTokenAddress] 
                += (2 ** 128) * winterPairAllocation / totalWinterLiquidity;
    }

    function receiveApproval(address from, uint256 tokens, address token, bytes calldata data) public override {
        data; // suppress unused variable compiler warnings
        receiveSeasonalTokens(from, token, tokens);
    }

    function receiveSeasonalTokens(address from, address tokenAddress, uint256 amount) public {
        
        allocateIncomingTokensToTradingPairs(tokenAddress, amount);

        emit Donate(from, tokenAddress, amount);

        ERC20Interface(tokenAddress).transferFrom(from, address(this), amount);
    }
    
    function onERC721Received(address _operator, address _from, uint256 liquidityTokenId, bytes memory _data) 
                             external override returns(bytes4) {

        require(msg.sender == address(nonfungiblePositionManager), 
                "Only Uniswap v3 liquidity tokens can be deposited");

        LiquidityToken memory liquidityToken = getLiquidityToken(liquidityTokenId);
        
        liquidityToken.owner = _from;
        liquidityToken.depositTime = block.timestamp;

        liquidityToken.position = tokenOfOwnerByIndex[_from].length;
        tokenOfOwnerByIndex[_from].push(liquidityTokenId);

        liquidityToken.initialCumulativeSpringTokensFarmed
            = cumulativeTokensFarmedPerUnitLiquidity[liquidityToken.seasonalToken][springTokenAddress];

        liquidityToken.initialCumulativeSummerTokensFarmed
            = cumulativeTokensFarmedPerUnitLiquidity[liquidityToken.seasonalToken][summerTokenAddress];

        liquidityToken.initialCumulativeAutumnTokensFarmed
            = cumulativeTokensFarmedPerUnitLiquidity[liquidityToken.seasonalToken][autumnTokenAddress];

        liquidityToken.initialCumulativeWinterTokensFarmed
            = cumulativeTokensFarmedPerUnitLiquidity[liquidityToken.seasonalToken][winterTokenAddress];

        liquidityTokens[liquidityTokenId] = liquidityToken;
        totalLiquidity[liquidityToken.seasonalToken] += liquidityToken.liquidity;

        emit Deposit(_from, liquidityTokenId);

        _data; _operator; // suppress unused variable compiler warnings
        return bytes4(keccak256("onERC721Received(address,address,uint256,bytes)"));
    }

    function getLiquidityToken(uint256 tokenId) internal view returns(LiquidityToken memory) {

        LiquidityToken memory liquidityToken;
        address token0;
        address token1;
        int24 tickLower;
        int24 tickUpper;
        uint256 liquidity;
        
        (token0, token1, tickLower, tickUpper, liquidity) = getPositionDataForLiquidityToken(tokenId);
        liquidityToken.liquidity = liquidity;
        
        if (token0 == wethAddress)
            liquidityToken.seasonalToken = token1;
        else if (token1 == wethAddress)
            liquidityToken.seasonalToken = token0;
        else
            revert("Invalid trading pair");

        if (liquidityToken.seasonalToken != springTokenAddress &&
            liquidityToken.seasonalToken != summerTokenAddress &&
            liquidityToken.seasonalToken != autumnTokenAddress &&
            liquidityToken.seasonalToken != winterTokenAddress)
            revert("Invalid trading pair");

        if (tickLower > MAXIMUM_TICK_LOWER || tickUpper < MINIMUM_TICK_UPPER)
            revert("Liquidity must cover full range of prices");

        return liquidityToken;
    }

    function getPositionDataForLiquidityToken(uint256 tokenId) 
                                             internal view returns (address, address, int24, int24, uint256){
        address token0;
        address token1;
        int24 tickLower;
        int24 tickUpper;
        uint256 liquidity;
        (,, token0, token1,, tickLower, tickUpper, liquidity,,,,) 
            = nonfungiblePositionManager.positions(tokenId);
        return (token0, token1, tickLower, tickUpper, liquidity);
    }

    function setCumulativeSpringTokensFarmedToCurrentValue(uint256 liquidityTokenId, address seasonalToken) internal {
        liquidityTokens[liquidityTokenId].initialCumulativeSpringTokensFarmed
            = cumulativeTokensFarmedPerUnitLiquidity[seasonalToken][springTokenAddress];
    }

    function setCumulativeSummerTokensFarmedToCurrentValue(uint256 liquidityTokenId, address seasonalToken) internal {
        liquidityTokens[liquidityTokenId].initialCumulativeSummerTokensFarmed
            = cumulativeTokensFarmedPerUnitLiquidity[seasonalToken][summerTokenAddress];
    }

    function setCumulativeAutumnTokensFarmedToCurrentValue(uint256 liquidityTokenId, address seasonalToken) internal {
        liquidityTokens[liquidityTokenId].initialCumulativeAutumnTokensFarmed
            = cumulativeTokensFarmedPerUnitLiquidity[seasonalToken][autumnTokenAddress];
    }

    function setCumulativeWinterTokensFarmedToCurrentValue(uint256 liquidityTokenId, address seasonalToken) internal {
        liquidityTokens[liquidityTokenId].initialCumulativeWinterTokensFarmed
            = cumulativeTokensFarmedPerUnitLiquidity[seasonalToken][winterTokenAddress];
    }

    function getPayoutSize(uint256 liquidityTokenId, address farmedSeasonalToken, 
                           address tradingPairSeasonalToken) internal view returns (uint256){

        uint256 initialCumulativeTokensFarmed;

        if (farmedSeasonalToken == springTokenAddress)
            initialCumulativeTokensFarmed = liquidityTokens[liquidityTokenId].initialCumulativeSpringTokensFarmed;
        else if (farmedSeasonalToken == summerTokenAddress)
            initialCumulativeTokensFarmed = liquidityTokens[liquidityTokenId].initialCumulativeSummerTokensFarmed;
        else if (farmedSeasonalToken == autumnTokenAddress)
            initialCumulativeTokensFarmed = liquidityTokens[liquidityTokenId].initialCumulativeAutumnTokensFarmed;
        else
            initialCumulativeTokensFarmed = liquidityTokens[liquidityTokenId].initialCumulativeWinterTokensFarmed;

        uint256 tokensFarmedPerUnitLiquiditySinceDeposit 
            = cumulativeTokensFarmedPerUnitLiquidity[tradingPairSeasonalToken][farmedSeasonalToken]
              - initialCumulativeTokensFarmed;

        return (tokensFarmedPerUnitLiquiditySinceDeposit 
                * liquidityTokens[liquidityTokenId].liquidity) / (2 ** 128);
    }

    function getPayoutSizes(uint256 liquidityTokenId) public view returns (uint256, uint256, uint256, uint256) {

        address tradingPairSeasonalToken = liquidityTokens[liquidityTokenId].seasonalToken;

        uint256 springPayout = getPayoutSize(liquidityTokenId, springTokenAddress, tradingPairSeasonalToken);
        uint256 summerPayout = getPayoutSize(liquidityTokenId, summerTokenAddress, tradingPairSeasonalToken);
        uint256 autumnPayout = getPayoutSize(liquidityTokenId, autumnTokenAddress, tradingPairSeasonalToken);
        uint256 winterPayout = getPayoutSize(liquidityTokenId, winterTokenAddress, tradingPairSeasonalToken);

        return (springPayout, summerPayout, autumnPayout, winterPayout);
    }

    function harvestSpring(uint256 liquidityTokenId, address tokenOwner, address tradingPairSeasonalToken) internal returns(uint256) {

        uint256 amount = getPayoutSize(liquidityTokenId, springTokenAddress, tradingPairSeasonalToken);
        setCumulativeSpringTokensFarmedToCurrentValue(liquidityTokenId, tradingPairSeasonalToken);
        return amount;
    }

    function harvestSummer(uint256 liquidityTokenId, address tokenOwner, address tradingPairSeasonalToken) internal returns(uint256) {

        uint256 amount = getPayoutSize(liquidityTokenId, summerTokenAddress, tradingPairSeasonalToken);
        setCumulativeSummerTokensFarmedToCurrentValue(liquidityTokenId, tradingPairSeasonalToken);
        return amount;
    }

    function harvestAutumn(uint256 liquidityTokenId, address tokenOwner, address tradingPairSeasonalToken) internal returns(uint256) {

        uint256 amount = getPayoutSize(liquidityTokenId, autumnTokenAddress, tradingPairSeasonalToken);
        setCumulativeAutumnTokensFarmedToCurrentValue(liquidityTokenId, tradingPairSeasonalToken);
        return amount;
    }

    function harvestWinter(uint256 liquidityTokenId, address tokenOwner, address tradingPairSeasonalToken) internal returns(uint256) {

        uint256 amount = getPayoutSize(liquidityTokenId, winterTokenAddress, tradingPairSeasonalToken);
        setCumulativeWinterTokensFarmedToCurrentValue(liquidityTokenId, tradingPairSeasonalToken);
        return amount;
    }

    function harvestAll(uint256 liquidityTokenId, address tokenOwner, address tradingPairSeasonalToken) 
            internal returns (uint256, uint256, uint256, uint256) {

        uint256 springAmount = harvestSpring(liquidityTokenId, tokenOwner, tradingPairSeasonalToken);
        uint256 summerAmount = harvestSummer(liquidityTokenId, tokenOwner, tradingPairSeasonalToken);
        uint256 autumnAmount = harvestAutumn(liquidityTokenId, tokenOwner, tradingPairSeasonalToken);
        uint256 winterAmount = harvestWinter(liquidityTokenId, tokenOwner, tradingPairSeasonalToken);

        return (springAmount, summerAmount, autumnAmount, winterAmount);
    }

    function sendHarvestedTokensToOwner(address tokenOwner, uint256 springAmount, uint256 summerAmount,
                                        uint256 autumnAmount, uint256 winterAmount) internal {

        if (springAmount > 0)
            ERC20Interface(springTokenAddress).transfer(tokenOwner, springAmount);
        if (summerAmount > 0)
            ERC20Interface(summerTokenAddress).transfer(tokenOwner, summerAmount);
        if (autumnAmount > 0)
            ERC20Interface(autumnTokenAddress).transfer(tokenOwner, autumnAmount);
        if (winterAmount > 0)
            ERC20Interface(winterTokenAddress).transfer(tokenOwner, winterAmount);
    }

    function harvest(uint256 liquidityTokenId) public {
        
        LiquidityToken storage liquidityToken = liquidityTokens[liquidityTokenId];
        require(msg.sender == liquidityToken.owner, "Only owner can harvest");
        
        (uint256 springAmount, 
         uint256 summerAmount,
         uint256 autumnAmount,
         uint256 winterAmount) = harvestAll(liquidityTokenId, 
                                         liquidityToken.owner, liquidityToken.seasonalToken);

        emit Harvest(liquidityToken.owner, liquidityTokenId, springAmount, summerAmount, autumnAmount, winterAmount);
        
        sendHarvestedTokensToOwner(liquidityToken.owner, springAmount, summerAmount, autumnAmount, winterAmount);
    }

    function canWithdraw(uint256 liquidityTokenId) public view returns (bool) {

        uint256 depositTime = liquidityTokens[liquidityTokenId].depositTime;
        uint256 timeSinceDepositTime = block.timestamp - depositTime;
        uint256 daysSinceDepositTime = timeSinceDepositTime / (24 * 60 * 60);

        return (daysSinceDepositTime) % (WITHDRAWAL_UNAVAILABLE_DAYS + WITHDRAWAL_AVAILABLE_DAYS) 
                    >= WITHDRAWAL_UNAVAILABLE_DAYS;
    }

    function nextWithdrawalTime(uint256 liquidityTokenId) public view returns (uint256) {
        
        uint256 depositTime = liquidityTokens[liquidityTokenId].depositTime;
        uint256 timeSinceDepositTime = block.timestamp - depositTime;
        uint256 withdrawalUnavailableTime = WITHDRAWAL_UNAVAILABLE_DAYS * 24 * 60 * 60;
        uint256 withdrawalAvailableTime = WITHDRAWAL_AVAILABLE_DAYS * 24 * 60 * 60;

        if (timeSinceDepositTime < withdrawalUnavailableTime)
            return depositTime + withdrawalUnavailableTime;

        uint256 numberOfWithdrawalCyclesUntilNextWithdrawalTime 
                    = 1 + (timeSinceDepositTime - withdrawalUnavailableTime) 
                          / (withdrawalUnavailableTime + withdrawalAvailableTime);

        return depositTime + withdrawalUnavailableTime 
                           + numberOfWithdrawalCyclesUntilNextWithdrawalTime
                             * (withdrawalUnavailableTime + withdrawalAvailableTime);
    }

    function withdraw(uint256 liquidityTokenId) external {

        require(canWithdraw(liquidityTokenId), "This token cannot be withdrawn at this time");

        LiquidityToken memory liquidityToken = liquidityTokens[liquidityTokenId];

        require(msg.sender == liquidityToken.owner, "Only owner can withdraw");

        harvest(liquidityTokenId);

        totalLiquidity[liquidityToken.seasonalToken] -= liquidityToken.liquidity;
        removeTokenFromListOfOwnedTokens(liquidityToken.owner, liquidityToken.position, liquidityTokenId);

        emit Withdraw(msg.sender, liquidityTokenId);

        nonfungiblePositionManager.safeTransferFrom(address(this), liquidityToken.owner, liquidityTokenId);
    }

    function removeTokenFromListOfOwnedTokens(address owner, uint256 index, uint256 liquidityTokenId) internal {

        // to remove an element from a list efficiently, we copy the last element in the list into the
        // position of the element we want to remove, and then remove the last element from the list

        uint256 length = tokenOfOwnerByIndex[owner].length;
        if (length > 1) {
            uint256 liquidityTokenIdOfLastTokenInList = tokenOfOwnerByIndex[owner][length - 1];
            LiquidityToken memory lastToken = liquidityTokens[liquidityTokenIdOfLastTokenInList];
            lastToken.position = index;
            tokenOfOwnerByIndex[owner][index] = liquidityTokenIdOfLastTokenInList;
            liquidityTokens[liquidityTokenIdOfLastTokenInList] = lastToken;
        }
        tokenOfOwnerByIndex[owner].pop();
        delete liquidityTokens[liquidityTokenId];
    }

}
        