(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-BOUNTY (err u101))
(define-constant ERR-BOUNTY-NOT-FOUND (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-PROOF-REQUIRED (err u104))
(define-constant ERR-ALREADY-CLAIMED (err u105))

(define-data-var contract-owner principal tx-sender)
(define-data-var total-trees-planted uint u0)
(define-data-var bounty-counter uint u0)

(define-map bounties
    uint
    {
        creator: principal,
        reward-per-tree: uint,
        total-reward: uint,
        remaining-reward: uint,
        trees-required: uint,
        trees-planted: uint,
        expiry: uint,
        active: bool,
    }
)

(define-map tree-proofs
    {
        bounty-id: uint,
        planter: principal,
    }
    {
        gps-lat: (string-utf8 50),
        gps-long: (string-utf8 50),
        photo-hash: (string-utf8 64),
        verified: bool,
        claimed: bool,
    }
)

(define-map planter-stats
    principal
    {
        total-trees: uint,
        total-rewards: uint,
    }
)

(define-read-only (get-bounty (bounty-id uint))
    (ok (unwrap! (map-get? bounties bounty-id) ERR-BOUNTY-NOT-FOUND))
)

(define-read-only (get-tree-proof
        (bounty-id uint)
        (planter principal)
    )
    (map-get? tree-proofs {
        bounty-id: bounty-id,
        planter: planter,
    })
)

(define-read-only (get-planter-stats (planter principal))
    (default-to {
        total-trees: u0,
        total-rewards: u0,
    }
        (map-get? planter-stats planter)
    )
)

(define-public (create-bounty
        (reward-per-tree uint)
        (trees-required uint)
        (expiry uint)
    )
    (let (
            (bounty-id (+ (var-get bounty-counter) u1))
            (total-reward (* reward-per-tree trees-required))
        )
        (try! (stx-transfer? total-reward tx-sender (as-contract tx-sender)))
        (map-set bounties bounty-id {
            creator: tx-sender,
            reward-per-tree: reward-per-tree,
            total-reward: total-reward,
            remaining-reward: total-reward,
            trees-required: trees-required,
            trees-planted: u0,
            expiry: expiry,
            active: true,
        })
        (var-set bounty-counter bounty-id)
        (ok bounty-id)
    )
)

(define-public (submit-tree-proof
        (bounty-id uint)
        (gps-lat (string-utf8 50))
        (gps-long (string-utf8 50))
        (photo-hash (string-utf8 64))
    )
    (let (
            (bounty (unwrap! (map-get? bounties bounty-id) (err ERR-BOUNTY-NOT-FOUND)))
            (current-height burn-block-height)
        )
        (asserts! (< current-height (get expiry bounty)) (err ERR-INVALID-BOUNTY))
        (asserts! (get active bounty) (err ERR-INVALID-BOUNTY))
        (map-set tree-proofs {
            bounty-id: bounty-id,
            planter: tx-sender,
        } {
            gps-lat: gps-lat,
            gps-long: gps-long,
            photo-hash: photo-hash,
            verified: false,
            claimed: false,
        })
        (ok true)
    )
)
(define-public (verify-tree-proof
        (bounty-id uint)
        (planter principal)
    )
    (let (
            (proof (unwrap!
                (map-get? tree-proofs {
                    bounty-id: bounty-id,
                    planter: planter,
                })
                (err ERR-PROOF-REQUIRED)
            ))
            (bounty (unwrap! (map-get? bounties bounty-id) (err ERR-BOUNTY-NOT-FOUND)))
        )
        (asserts! (is-eq tx-sender (var-get contract-owner))
            (err ERR-NOT-AUTHORIZED)
        )
        (map-set tree-proofs {
            bounty-id: bounty-id,
            planter: planter,
        }
            (merge proof { verified: true })
        )
        (map-set bounties bounty-id
            (merge bounty { trees-planted: (+ (get trees-planted bounty) u1) })
        )
        (var-set total-trees-planted (+ (var-get total-trees-planted) u1))
        (ok true)
    )
)
(define-public (claim-reward (bounty-id uint))
    (let (
            (proof (unwrap!
                (map-get? tree-proofs {
                    bounty-id: bounty-id,
                    planter: tx-sender,
                })
                (err ERR-PROOF-REQUIRED)
            ))
            (bounty (unwrap! (map-get? bounties bounty-id) (err ERR-BOUNTY-NOT-FOUND)))
            (stats (default-to {
                total-trees: u0,
                total-rewards: u0,
            }
                (map-get? planter-stats tx-sender)
            ))
            (reward (get reward-per-tree bounty))
        )
        (asserts! (get verified proof) (err ERR-PROOF-REQUIRED))
        (asserts! (not (get claimed proof)) (err ERR-ALREADY-CLAIMED))
        (asserts! (<= reward (get remaining-reward bounty))
            (err ERR-INSUFFICIENT-FUNDS)
        )
        (let ((transfer-result (stx-transfer? reward (as-contract tx-sender) tx-sender)))
            (match transfer-result
                success (begin
                    (map-set tree-proofs {
                        bounty-id: bounty-id,
                        planter: tx-sender,
                    }
                        (merge proof { claimed: true })
                    )
                    (map-set bounties bounty-id
                        (merge bounty { remaining-reward: (- (get remaining-reward bounty) reward) })
                    )
                    (map-set planter-stats tx-sender {
                        total-trees: (+ (get total-trees stats) u1),
                        total-rewards: (+ (get total-rewards stats) reward),
                    })
                    (ok true)
                )
                error (err ERR-INSUFFICIENT-FUNDS)
            )
        )
    )
)
(define-public (close-bounty (bounty-id uint))
    (let (
            (bounty (unwrap! (map-get? bounties bounty-id) (err ERR-BOUNTY-NOT-FOUND)))
            (remaining (get remaining-reward bounty))
        )
        (asserts! (is-eq tx-sender (get creator bounty)) (err ERR-NOT-AUTHORIZED))
        (match (stx-transfer? remaining (as-contract tx-sender) (get creator bounty))
            success (begin
                (map-set bounties bounty-id
                    (merge bounty {
                        active: false,
                        remaining-reward: u0,
                    })
                )
                (ok true)
            )
            error (err ERR-INSUFFICIENT-FUNDS)
        )
    )
)
