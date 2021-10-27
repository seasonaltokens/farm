import pytest
from brownie import TestSpringToken, TestNftPositionManager, TestSeasonalTokenFarm
from brownie import accounts, chain, reverts


@pytest.fixture
def spring(accounts, chain):
    spring = TestSpringToken.deploy({'from': accounts[0]})
    return spring

@pytest.fixture
def summer(accounts, chain):
    summer = TestSpringToken.deploy({'from': accounts[0]})
    return summer

@pytest.fixture
def autumn(accounts, chain):
    autumn = TestSpringToken.deploy({'from': accounts[0]})
    return autumn

@pytest.fixture
def winter(accounts, chain):
    winter = TestSpringToken.deploy({'from': accounts[0]})
    return winter

@pytest.fixture
def weth(accounts, chain):
    weth = TestSpringToken.deploy({'from': accounts[0]})
    return weth

@pytest.fixture
def position_manager(accounts, chain):
    position_manager = TestNftPositionManager.deploy({'from': accounts[0]})
    return position_manager

@pytest.fixture
def farm(accounts, position_manager, spring, summer, autumn, winter, weth):
    farm = TestSeasonalTokenFarm.deploy(position_manager.address, 
                                        spring.address, summer.address, autumn.address, winter.address, 
                                        weth.address, chain.time() + 120 * 24 * 60 * 60,
                                        {'from': accounts[0]})
    return farm

def test_initial_total_allocation_size_is_zero(farm):
    assert farm.getEffectiveTotalAllocationSize(0,0,0,0) == 0

def allocation_sizes(farm):
    return (farm.springAllocationSize(),
            farm.summerAllocationSize(), 
            farm.autumnAllocationSize(),
            farm.winterAllocationSize())

def test_reallocations(farm):
    assert farm.numberOfReallocations() == 0
    assert allocation_sizes(farm) == (5, 6, 7, 8)
    chain.sleep(farm.REALLOCATION_INTERVAL() + 120 * 24 * 60 * 60)
    chain.mine(1)
    assert farm.numberOfReallocations() == 1
    assert allocation_sizes(farm) == (10, 6, 7, 8)
    chain.sleep(farm.REALLOCATION_INTERVAL())
    chain.mine(1)
    assert farm.numberOfReallocations() == 2
    assert allocation_sizes(farm) == (10, 12, 7, 8)
    chain.sleep(farm.REALLOCATION_INTERVAL())
    chain.mine(1)
    assert farm.numberOfReallocations() == 3
    assert allocation_sizes(farm) == (10, 12, 14, 8)
    chain.sleep(farm.REALLOCATION_INTERVAL())
    chain.mine(1)
    assert farm.numberOfReallocations() == 4
    assert allocation_sizes(farm) == (5, 6, 7, 8)

def test_effective_total_allocation_size(farm):
    assert allocation_sizes(farm) == (5, 6, 7, 8)
    assert farm.getEffectiveTotalAllocationSize(0, 0, 0, 0) == 0
    assert farm.getEffectiveTotalAllocationSize(1, 0, 0, 0) == 5
    assert farm.getEffectiveTotalAllocationSize(0, 1, 0, 0) == 6
    assert farm.getEffectiveTotalAllocationSize(0, 0, 1, 0) == 7
    assert farm.getEffectiveTotalAllocationSize(0, 0, 0, 1) == 8
    assert farm.getEffectiveTotalAllocationSize(1, 1, 1, 1) == 5 + 6 + 7 + 8

def test_revert_donate_with_no_liquidity_in_farm(accounts, farm, winter):
    winter.setBalance(accounts[0].address, int(10**18))
    with reverts():
        winter.safeApproveAndCall(farm.address, 0, int(10**18), bytes(), {'from': accounts[0]})
    
def test_create_liquidity_token(accounts, position_manager, weth, winter):
    assert position_manager.numberOfTokens() == 0
    liquidity_token_id = position_manager.numberOfTokens()
    position_manager.createLiquidityToken(accounts[0].address, weth.address, winter.address, 
                                          -887200, 887200, 10000000000, {'from': accounts[0]})
    assert position_manager.numberOfTokens() == 1

@pytest.fixture
def position_manager_with_liquidity_token(accounts, position_manager, weth, winter):
    liquidity_token_id = position_manager.numberOfTokens()
    position_manager.createLiquidityToken(accounts[0].address, weth.address, winter.address, 
                                          -887200, 887200, 10000000000, {'from': accounts[0]})
    return position_manager

def test_deposit_liquidity_token(accounts, position_manager_with_liquidity_token, farm):
    liquidity_token_id = position_manager_with_liquidity_token.numberOfTokens() - 1
    position_manager_with_liquidity_token.safeTransferFrom(accounts[0].address, 
                                                           farm.address, liquidity_token_id, 
                                                           {'from': accounts[0]})
    assert farm.balanceOf(accounts[0].address) == 1
    assert farm.tokenOfOwnerByIndex(accounts[0].address, 0) == liquidity_token_id

def test_deposit_revert_weth_not_in_trading_pair(accounts, position_manager, farm, spring, summer):
    position_manager.createLiquidityToken(accounts[0].address, spring.address, summer.address, 
                                          -887200, 887200, 10000000000, {'from': accounts[0]})
    with reverts("Invalid trading pair"):
        position_manager.safeTransferFrom(accounts[0].address, farm.address, 0, {'from': accounts[0]})

def test_deposit_revert_seasonal_token_not_in_trading_pair(accounts, position_manager, farm, weth):
    position_manager.createLiquidityToken(accounts[0].address, weth.address, weth.address, 
                                          -887200, 887200, 10000000000, {'from': accounts[0]})
    with reverts("Invalid trading pair"):
        position_manager.safeTransferFrom(accounts[0].address, farm.address, 0, {'from': accounts[0]})

def test_deposit_revert_not_full_range(accounts, position_manager, farm, weth, spring):
    position_manager.createLiquidityToken(accounts[0].address, spring.address, weth.address, 
                                          -887100, 887200, 10000000000, {'from': accounts[0]})
    position_manager.createLiquidityToken(accounts[0].address, spring.address, weth.address, 
                                          -887200, 887100, 10000000000, {'from': accounts[0]})
    with reverts("Liquidity must cover full range of prices"):
        position_manager.safeTransferFrom(accounts[0].address, farm.address, 0, {'from': accounts[0]})
    with reverts("Liquidity must cover full range of prices"):
        position_manager.safeTransferFrom(accounts[0].address, farm.address, 1, {'from': accounts[0]})

def test_deposit_revert_not_uniswap_v3_token(accounts, position_manager, farm, weth, spring):
    position_manager_2 = TestNftPositionManager.deploy({'from': accounts[0]})
    position_manager_2.createLiquidityToken(accounts[0].address, weth.address, spring.address, 
                                            -887200, 887200, 10000000000, {'from': accounts[0]})
    with reverts("Only Uniswap v3 liquidity tokens can be deposited"):
        position_manager_2.safeTransferFrom(accounts[0].address, farm.address, 0, {'from': accounts[0]})


@pytest.fixture
def farm_with_deposit(accounts, position_manager_with_liquidity_token, farm):
    liquidity_token_id = position_manager_with_liquidity_token.numberOfTokens() - 1
    position_manager_with_liquidity_token.safeTransferFrom(accounts[0].address, 
                                                           farm.address, liquidity_token_id, 
                                                           {'from': accounts[0]})
    return farm


def test_donate(accounts, farm_with_deposit, winter):
    winter.setBalance(accounts[0].address, int(10**18))
    winter.safeApproveAndCall(farm_with_deposit.address, 0, int(10**18), bytes(), {'from': accounts[0]})
    assert winter.balanceOf(accounts[0].address) == 0
    assert winter.balanceOf(farm_with_deposit.address) == int(10**18)


@pytest.fixture
def farm_with_donation(accounts, farm_with_deposit, winter):
    winter.setBalance(accounts[0].address, int(10**18))
    winter.safeApproveAndCall(farm_with_deposit.address, 0, int(10**18), bytes(), {'from': accounts[0]})
    return farm_with_deposit


def test_tokens_available_for_harvest(accounts, position_manager_with_liquidity_token, farm_with_donation, winter, spring):
    liquidity_token_id = position_manager_with_liquidity_token.numberOfTokens() - 1
    assert farm_with_donation.cumulativeTokensFarmedPerUnitLiquidity(winter.address, winter.address) > 0
    assert farm_with_donation.cumulativeTokensFarmedPerUnitLiquidity(spring.address, winter.address) == 0
    assert farm_with_donation.cumulativeTokensFarmedPerUnitLiquidity(winter.address, spring.address) == 0
    assert farm_with_donation.getPayoutSizes(liquidity_token_id)[3] != 0

def test_harvest(accounts, position_manager_with_liquidity_token, farm_with_donation, winter, spring):
    liquidity_token_id = position_manager_with_liquidity_token.numberOfTokens() - 1
    farm_with_donation.harvest(liquidity_token_id, {'from': accounts[0]})
    assert abs(winter.balanceOf(accounts[0].address) - int(10**18)) < 10 # small rounding errors are expected
    assert winter.balanceOf(farm_with_donation.address) < 10
    # nothing left to harvest
    assert farm_with_donation.getPayoutSizes(liquidity_token_id) == (0, 0, 0, 0)

def test_harvest_revert_not_owner(accounts, position_manager_with_liquidity_token, farm_with_donation):
    liquidity_token_id = position_manager_with_liquidity_token.numberOfTokens() - 1
    with reverts():
        farm_with_donation.harvest(liquidity_token_id, {'from': accounts[1]})
    

def test_withdraw(accounts, position_manager_with_liquidity_token, farm_with_donation, winter):
    liquidity_token_id = position_manager_with_liquidity_token.numberOfTokens() - 1
    assert farm_with_donation.balanceOf(accounts[0].address) == 1
    chain.sleep(farm_with_donation.WITHDRAWAL_UNAVAILABLE_DAYS() * 24 * 60 * 60)
    farm_with_donation.withdraw(liquidity_token_id, {'from': accounts[0]})
    assert farm_with_donation.balanceOf(accounts[0].address) == 0

def test_revert_withdraw_not_owner(accounts, position_manager_with_liquidity_token, 
                                   farm_with_donation, winter):
    liquidity_token_id = position_manager_with_liquidity_token.numberOfTokens() - 1
    chain.sleep(farm_with_donation.WITHDRAWAL_UNAVAILABLE_DAYS() * 24 * 60 * 60)
    with reverts():
        farm_with_donation.withdraw(liquidity_token_id, {'from': accounts[1]})

def test_revert_withdrawal_unavailable(accounts, position_manager_with_liquidity_token, 
                                       farm_with_donation, winter):
    liquidity_token_id = position_manager_with_liquidity_token.numberOfTokens() - 1
    with reverts():
        farm_with_donation.withdraw(liquidity_token_id, {'from': accounts[1]})

def test_next_withdrawal_time(accounts, position_manager_with_liquidity_token, farm_with_donation, winter):
    liquidity_token_id = position_manager_with_liquidity_token.numberOfTokens() - 1
    withdrawal_time = (farm_with_donation.liquidityTokens(liquidity_token_id)[2]
                        + farm_with_donation.WITHDRAWAL_UNAVAILABLE_DAYS() * 24 *60 * 60)
    
    while withdrawal_time <= chain.time():
        withdrawal_time += (farm_with_donation.WITHDRAWAL_UNAVAILABLE_DAYS()
                            + farm_with_donation.WITHDRAWAL_AVAILABLE_DAYS()) * 24 * 60 * 60
    
    assert farm_with_donation.nextWithdrawalTime(liquidity_token_id) == withdrawal_time

    chain.sleep(farm_with_donation.WITHDRAWAL_UNAVAILABLE_DAYS() * 24 * 60 * 60)
    chain.mine(1)

    while withdrawal_time <= chain.time():
        withdrawal_time += (farm_with_donation.WITHDRAWAL_UNAVAILABLE_DAYS()
                            + farm_with_donation.WITHDRAWAL_AVAILABLE_DAYS()) * 24 * 60 * 60
    
    assert farm_with_donation.nextWithdrawalTime(liquidity_token_id) == withdrawal_time


@pytest.fixture
def position_manager_with_four_liquidity_tokens(accounts, position_manager, 
                                                weth, spring, summer, autumn, winter):
    position_manager.createLiquidityToken(accounts[0].address, weth.address, spring.address, 
                                          -887200, 887200, 10000000000, {'from': accounts[0]})
    position_manager.createLiquidityToken(accounts[0].address, weth.address, summer.address, 
                                          -887200, 887200, 10000000000, {'from': accounts[0]})
    position_manager.createLiquidityToken(accounts[0].address, weth.address, autumn.address, 
                                          -887200, 887200, 10000000000, {'from': accounts[0]})
    position_manager.createLiquidityToken(accounts[0].address, weth.address, winter.address, 
                                          -887200, 887200, 10000000000, {'from': accounts[0]})
    return position_manager


@pytest.fixture
def farm_with_liquidity_in_three_pairs(accounts, position_manager_with_four_liquidity_tokens, farm):
    position_manager_with_four_liquidity_tokens.safeTransferFrom(accounts[0].address, farm.address, 0,
                                                                 {'from': accounts[0]})
    position_manager_with_four_liquidity_tokens.safeTransferFrom(accounts[0].address, farm.address, 1,
                                                                 {'from': accounts[0]})
    position_manager_with_four_liquidity_tokens.safeTransferFrom(accounts[0].address, farm.address, 2,
                                                                 {'from': accounts[0]})
    return farm


def test_harvest_from_farm_with_donations_and_liquidity_in_three_pairs(accounts, farm_with_liquidity_in_three_pairs, 
                                                      spring, summer, autumn, winter):
    spring.setBalance(accounts[0].address, int(10**18))
    spring.safeApproveAndCall(farm_with_liquidity_in_three_pairs.address, 0, int(10**18), bytes(), 
                              {'from': accounts[0]})
    assert spring.balanceOf(accounts[0].address) == 0
    assert spring.balanceOf(farm_with_liquidity_in_three_pairs.address) == int(10**18)
    summer.setBalance(accounts[0].address, int(10**18))
    summer.safeApproveAndCall(farm_with_liquidity_in_three_pairs.address, 0, int(10**18), bytes(), 
                              {'from': accounts[0]})
    assert summer.balanceOf(accounts[0].address) == 0
    assert summer.balanceOf(farm_with_liquidity_in_three_pairs.address) == int(10**18)
    autumn.setBalance(accounts[0].address, int(10**18))
    autumn.safeApproveAndCall(farm_with_liquidity_in_three_pairs.address, 0, int(10**18), bytes(), 
                              {'from': accounts[0]})
    assert autumn.balanceOf(accounts[0].address) == 0
    assert autumn.balanceOf(farm_with_liquidity_in_three_pairs.address) == int(10**18)

    assert farm_with_liquidity_in_three_pairs.getPayoutSizes(0) != (0, 0, 0, 0)
    farm_with_liquidity_in_three_pairs.harvest(0, {'from': accounts[0]})
    assert farm_with_liquidity_in_three_pairs.getPayoutSizes(0) == (0, 0, 0, 0)


def test_withdraw_from_farm_with_liquidity_in_three_pairs(accounts, farm_with_liquidity_in_three_pairs):
    assert farm_with_liquidity_in_three_pairs.balanceOf(accounts[0].address) == 3
    assert farm_with_liquidity_in_three_pairs.tokenOfOwnerByIndex(accounts[0].address, 0) == 0
    assert farm_with_liquidity_in_three_pairs.tokenOfOwnerByIndex(accounts[0].address, 1) == 1
    assert farm_with_liquidity_in_three_pairs.tokenOfOwnerByIndex(accounts[0].address, 2) == 2

    chain.sleep(farm_with_liquidity_in_three_pairs.WITHDRAWAL_UNAVAILABLE_DAYS() * 24 * 60 * 60)

    farm_with_liquidity_in_three_pairs.withdraw(0, {'from': accounts[0]})
    assert farm_with_liquidity_in_three_pairs.balanceOf(accounts[0].address) == 2
    assert farm_with_liquidity_in_three_pairs.tokenOfOwnerByIndex(accounts[0].address, 0) == 2
    assert farm_with_liquidity_in_three_pairs.tokenOfOwnerByIndex(accounts[0].address, 1) == 1

    farm_with_liquidity_in_three_pairs.withdraw(1, {'from': accounts[0]})
    assert farm_with_liquidity_in_three_pairs.balanceOf(accounts[0].address) == 1
    assert farm_with_liquidity_in_three_pairs.tokenOfOwnerByIndex(accounts[0].address, 0) == 2