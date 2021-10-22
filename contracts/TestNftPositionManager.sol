// Seasonal Token Farm Test NFT Position Manager

//SPDX-License-Identifier: MIT
pragma solidity 0.8.5;
pragma abicoder v2;

import "../interfaces/ERC721TokenReceiver.sol";


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

contract TestNftPositionManager is INonfungiblePositionManager {

    uint256 public numberOfTokens;
    
    mapping(uint => address) public operators;
    mapping(uint => address) public token0s;
    mapping(uint => address) public token1s;
    mapping(uint => int24) public tickLowers;
    mapping(uint => int24) public tickUppers;
    mapping(uint => uint128) public liquidities;

    function createLiquidityToken(address operator, 
                                  address token0, 
                                  address token1, 
                                  int24 tickLower, 
                                  int24 tickUpper,
                                  uint128 liquidity) public returns (uint256) {

        uint256 tokenId = numberOfTokens;

        operators[tokenId] = operator;
        token0s[tokenId] = token0;
        token1s[tokenId] = token1;
        tickLowers[tokenId] = tickLower;
        tickUppers[tokenId] = tickUpper;
        liquidities[tokenId] = liquidity;

        numberOfTokens++;

        return tokenId;
    }

    function positions(uint256 tokenId)
        external override 
        view
        returns (uint96 nonce,
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
                 uint128 tokensOwed1) {

        operator = operators[tokenId];
        token0 = token0s[tokenId];
        token1 = token1s[tokenId];
        tickLower = tickLowers[tokenId];
        tickUpper = tickUppers[tokenId];
        liquidity = liquidities[tokenId];

        return (0, operator, token0, token1, 
                0, tickLower, tickUpper, liquidity, 0, 0, 0, 0);
    }

    function safeTransferFrom(address _from, address _to, uint256 _tokenId) external override payable {
        bytes memory data;
        require(_from == operators[_tokenId], "Only owner can transfer");
        operators[_tokenId] = _to;
        if (isContract(_to))
            require(ERC721TokenReceiver(_to).onERC721Received(operators[_tokenId], _from, _tokenId, data)
                      == bytes4(keccak256("onERC721Received(address,address,uint256,bytes)")), 
                    "onERC721Received failed.");
    }

    function isContract(address _addr) private returns (bool isContract){
        uint32 size;
        assembly {
            size := extcodesize(_addr)
        }
        return (size > 0);
    }
}