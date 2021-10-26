# Certification minter

The certification token minter has 4 redeemers [Initialise, Update, Copy, Destroy]

## Initialise
![../diagrams/Certification/Initialise.jpg](../diagrams/Certification/Initialise.jpg)
Initialise creates a one shot CertificationAuthority NFT using the TxOutRef from a UTXO parameterising the minter to guarantee uniqueness.

## Update
![../diagrams/Certification/Update.jpg](../diagrams/Certification/Update.jpg)
Update requires the presence of the CertificationAuthority NFT and a CertifiedDatum. This allows the minting of a CertificationToken for the CertifiedDatum. This token carries an Ada requirement and the CertificationTokenCopyingParameters which tell future copiers who to return this Ada to.

## Copy
![../diagrams/Certification/Copy.jpg](../diagrams/Certification/Copy.jpg)
Copy allows exponential proliferation of a CertificationToken. The Ada locked with the token must be returned to its owner and further copies must be minted in order to extract a CertificationToken. The copies produced can be made to pay back to the creator of the Copy transaction.

## Destroy
![../diagrams/Certification/Destroy.jpg](../diagrams/Certification/Destroy.jpg)
Destroy allows destruction of a CertificationToken residing at the minter address once the expiration date of the token has passed (this is stored in the CertificationTokenCopyingParameters carried with the token). The Ada must be payed back to the creator of the token.

### Legend
Hopefully this helps to explain the notation.
![../diagrams/Certification/Legend.jpg](../diagrams/Certification/Legend.jpg)
