# cardano-royalties-contracts
## Description
This a smart contract example for **Royalties** developed using Plutus and Haskell 
during my internship period at [Sireto Tech](https://sireto.com/).
The royalty concepts implemented here are simple and similar to real life Books Royalty i.e Royalty amount is distributed only
once when the Marketplace sells the token minted by the creators.


The basic functionality goes like:

```mermaid
graph TD;
A[Creators mint the token] --> B[Creators put token for delegation including royalty share for each creator];
B[Creators put token for delegation including royalty share for each creator] --> C[Delegated token is bought by Marketplaces];
C[Delegated token is bought by Marketplaces] --> D[Marketplaces put token for the Sale];
D[Marketplaces put token for the Sale] --> E[Buyers buy the token from marketplace];
E[Buyers buy the token from marketplace] --> F[Royalty is distributed based on creators' share];
```
The contract has been run and tested in `EmulatorTrace`. 
