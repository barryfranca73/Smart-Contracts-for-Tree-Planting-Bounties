# Smart Contracts for Tree Planting Bounties
A Clarity smart contract for incentivizing and tracking tree planting activities through bounties and proof verification.

## 🎯 Features

- Create tree planting bounties with STX rewards
- Submit GPS coordinates and photo proof of planted trees
- Verify submitted proofs by contract owner
- Claim rewards for verified tree plantings
- Track planter statistics and total impact

## 📋 Contract Functions

### For Bounty Creators
- `create-bounty`: Create a new bounty with specified reward per tree
- `close-bounty`: Close an existing bounty and retrieve remaining funds

### For Tree Planters
- `submit-tree-proof`: Submit proof of planted trees with GPS and photo
- `claim-reward`: Claim STX rewards for verified trees

### For Contract Owner
- `verify-tree-proof`: Verify submitted tree proofs

### Read-Only Functions
- `get-bounty`: Get details of a specific bounty
- `get-tree-proof`: Get proof details for a planter
- `get-planter-stats`: Get statistics for a tree planter

## 🚀 Usage Example

1. Create a bounty:
```clarity
(contract-call? .tree-planting-bounties create-bounty u1000 u10 u1000)
```

2. Submit tree proof:
```clarity
(contract-call? .tree-planting-bounties submit-tree-proof u1 "40.7128" "-74.0060" "QmHash...")
```

3. Verify proof (contract owner only):
```clarity
(contract-call? .tree-planting-bounties verify-tree-proof u1 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

4. Claim reward:
```clarity
(contract-call? .tree-planting-bounties claim-reward u1)
```

## 🌿 Environmental Impact

Track the total number of trees planted through the contract using:
```clarity
(contract-call? .tree-planting-bounties get-total-trees-planted)
```

## 🔒 Security

- All functions include appropriate authorization checks
- Proof verification required before reward claims
- Safe STX transfer handling
```

