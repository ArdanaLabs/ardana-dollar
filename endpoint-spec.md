# Ardana-dollar Endpoint specification

This document describes the endpoint interfaces and properties for the ardana-dollar project.

This allows us to describe the behaviors seen in the Solidity source implementation (MakerDAI), and create a simple plutus interface to match

As endpoints are implemented, we will add additional information including graph-viz state machine diagrams, example calls and expected outputs, etc.

Currently, this provides information relating to the base prototype we want to have in place for the end of July, relating to the foundational stablecoin operations (minting/redeeming)

once these endpoints are in place and have test coverage/demo runs, we will add Liquidation features, Governance features, and an oracle that is more than simply stubbed out.

These contracts will distribute, Mint,  two main tokens:
1) dUSD - the Ardana Stablecoin
2) DANA - the Ardana Governance token (minted seperately)

There is also an Ardana Decentralized Exchange effort, which will support many other stablecoins.

## Governance Contract

The Governance Contract defines the types of actions the DAO can take and the democratic systems that drive it.

for now, it will directly instantiate Vaults.

at launch time, the `Init-Vault` endpoint will be replaced with:
  i) initiailization that will run once at launch
  %% ii) voting systems which, when successful, can update contract parameters and perform  other tasks
  (voting systems will be part of a v2)
  
  governance will also include reward pooling/ stake pooling for DANA holders
   
   
there can only ever be a single governance contract instance, otherwise there is a vulnerability

### Init-Vault

prerequisite: none

input: 
VaultConfig :: { supportedToken :: AssetClass, minCollateralRatio :: Rational }

expected behaviour: 

calling this endpoint instantiates a Vault Contract with `VaultConfig` as its config params, such that the user can then activate the Vault Contract with their Wallet and immediately call `MintUSD`

this is a temporary convenience endpoint that will be removed from the public interface prior to launch.

however the underlying code will be recycled as an initial configuration to run at deployment time,


## Vault Contract

The Vault contract a single supported collateral type whose value can be obtained through a reliable oracle. Which collateral types are supported is decided by the Governance system.  for now this can be stubbed out as some configuration used in the contract, which can be changed.  
Vault Config values:
supportedToken :: AssetClass - tokens permitted for collateral use.
minCollateralRatio :: Rational - default: 2/3 - the minimum borrow/collateral value ratio permitted before liquidating an account's assets
(other config values may be added for fee calculations, etc)

In practice, Collaterial Ratios are encouraged to be much higher than the minimum, more like 1:3 - 1:10. This insulates borrowers from price volatility and prevents liquidation events.

In order to compare collateral to the value of USD, we will require an Oracle.  At this time, oracles must be stubbed out as we do not yet have a solid source of information on chain

Users can lock supported assets into their vault minting dUSD at a given collateral Ratio, users can return dUSD to the contract along with a collateral ratio to be maintained after the redemption (in the case of a partial redemption). This is also called a Collateralized debt position.

There will be many vault contract instances, each with a unique `supportedToken` - all of which mint dUSD.

the `supportedToken` for a vault may also be referred to as the underlying token of a vault.

### MintUSD

prerequisites: Vault for token must be instantiated, user must have required amount of token in the wallet
the user may or may not have pre-existing CDP's (collateralized debt postions)

input:
MintUSDReq :: { dUSDQty :: Integer, collateralRatio :: Rational }
dUSDQty - the amount of dUSD the user wants
collateralRatio - the collateralRatio that should be maintained for the user's account

expected behavior:

let `exr` equal the current exchange rate of underlying : USD
let `underlyingToMaxLoan` be a function on x -> (x / `collateralRatio) * exr
let `maxWalletAmount` equal `underlyingToMaxLoan` applied to the user's wallet balance of the underlying (or the UTXO's supplied by the user)
let `maxVaultAmount` equal (`underlyingToMaxLoan` applied to any pre-existing collateral in the Vault contract prior to this call) minus any existing dUSD loans prior to this call
let `maxLoanSize` equal the sum of `maxWalletAmount` and `maxVaultAmount`

let `mintQty` equal the LOWEST of:
- `maxLoanSize`
- `dUSDQty` input parameter
let `payQty` equal the (`mintQty` / exr) * `collateralRatio` parameter

the user's wallet balance will be reduced by `payQty` of underlying, plus fees in Ada.
the user's wallet balance will be increased by `mintQty` of dUSD.

invariant inputs/behaviors

if the `exr` remains constant, it is impossible for the user to have a collateralRatio below the one specified (or the Vault's minimum collateralRatio) on the block following this action.  we must always round down values to maintain minimum and user-set collateralRatio (whichever is HIGHER).

passing negative values  for `dUSDQty` and `collateralRatio` is not permitted (`collateralRatio` is lower-bounded by the `minColateralRatio`)

under no circumstances (ie, negative input) can this endpoint RELEASE underlying tokens back to the user, or burn tokens of any kind.

### Redeem

prerequisites:
the user's wallet must have dUSD (implication: User called `MintUSD`)

input:
RedeemReq :: { dUSDQty :: Integer, collateralRatio :: Rational }
dUSDQty - the amount of dUSD the user wants to redeem for the underlying token
collateralRatio - the collateralRatio that should be maintained for the user's account

expected behavior:
interest/stability fees must be advanced before further calculations run

let `exr` equal the current exchange rate of underlying : USD
let `getMaxRedeem` be a function on x -> (x * `collateralRatio)
let `providedUSD` equal the LOWEST of:
- `dUSDQty`
- total dUSD in user's wallet/provided utxos
let `newLoanAmount` equal pre-existing loan amount after interest/stability fees, minus providedUSD
let `newLoanAmountUnderlying` equal `newLoanAmount` / `exr` 
let `maxRedeem` equal `getMaxRedeem` applied to the total collateral for this user in this vault, minus `newLoanAmountUnderlying`
let `redeemQty` equal the LOWEST of
- `maxRedeem`
- `dUSDQty` / `exr`

the user's wallet balance will decrease by `providedUSD` in dUSD, this is burned.
the user's wallet balance will increase by `redeemQty` in the underlying token

invariant inputs/behaviors:

If the `exr` remains constant, it is impossible for the user to have a collateralRatio below the one specified (or the Vault's minimum collateralRatio) on the block following this action.  we must always round down values to maintain minimum and user-set collateralRatio (whichever is HIGHER)

Passing negative values  for `dUSDQty` and `collateralRatio` is not permitted (`collateralRatio` is lower-bounded by the `minColateralRatio`)

Under no circumstances (ie, negative input) can this endpoint mint dUSD tokens or distribute them to the user.

This must be called by the user who took out the loan in question, any other address should not be able to extract funds and should not see a change in wallet balances beyond transaction fees.




