(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-BOUNTY (err u101))
(define-constant ERR-BOUNTY-NOT-FOUND (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-PROOF-REQUIRED (err u104))
(define-constant ERR-ALREADY-CLAIMED (err u105))
(define-constant ERR-ALREADY-VOTED (err u106))
(define-constant ERR-INSUFFICIENT-VERIFICATIONS (err u107))

(define-data-var contract-owner principal tx-sender)
(define-data-var total-trees-planted uint u0)
(define-data-var bounty-counter uint u0)
(define-data-var min-verifications uint u3)
(define-data-var base-reputation uint u100)
(define-data-var max-reputation uint u1000)

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
        verification-count: uint,
    }
)

(define-map community-verifications
    {
        bounty-id: uint,
        planter: principal,
        verifier: principal,
    }
    {
        vote: bool,
        block-height: uint,
    }
)

(define-map planter-stats
    principal
    {
        total-trees: uint,
        total-rewards: uint,
    }
)

(define-map user-reputation
    principal
    {
        score: uint,
        correct-verifications: uint,
        total-verifications: uint,
        last-updated: uint,
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

(define-read-only (get-user-reputation (user principal))
    (default-to {
        score: (var-get base-reputation),
        correct-verifications: u0,
        total-verifications: u0,
        last-updated: u0,
    }
        (map-get? user-reputation user)
    )
)

(define-read-only (calculate-reputation-weight (user principal))
    (let ((reputation (get-user-reputation user)))
        (/ (get score reputation) (var-get base-reputation))
    )
)

(define-read-only (get-community-verification
        (bounty-id uint)
        (planter principal)
        (verifier principal)
    )
    (map-get? community-verifications {
        bounty-id: bounty-id,
        planter: planter,
        verifier: verifier,
    })
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
            verification-count: u0,
        })
        (ok true)
    )
)

(define-public (community-verify-tree
        (bounty-id uint)
        (planter principal)
        (vote bool)
    )
    (let (
            (proof (unwrap!
                (map-get? tree-proofs {
                    bounty-id: bounty-id,
                    planter: planter,
                })
                (err ERR-PROOF-REQUIRED)
            ))
            (existing-vote (map-get? community-verifications {
                bounty-id: bounty-id,
                planter: planter,
                verifier: tx-sender,
            }))
            (current-count (get verification-count proof))
            (verifier-weight (calculate-reputation-weight tx-sender))
            (weighted-vote (if vote
                verifier-weight
                u0
            ))
            (new-count (+ current-count weighted-vote))
        )
        (asserts! (is-none existing-vote) (err ERR-ALREADY-VOTED))
        (asserts! (not (is-eq tx-sender planter)) (err ERR-NOT-AUTHORIZED))
        (map-set community-verifications {
            bounty-id: bounty-id,
            planter: planter,
            verifier: tx-sender,
        } {
            vote: vote,
            block-height: burn-block-height,
        })
        (begin
            (unwrap-panic (update-verifier-reputation tx-sender vote))
            (if vote
                (begin
                    (map-set tree-proofs {
                        bounty-id: bounty-id,
                        planter: planter,
                    }
                        (merge proof { verification-count: new-count })
                    )
                    (if (>= new-count
                            (* (var-get min-verifications)
                                (var-get base-reputation)
                            ))
                        (begin
                            (map-set tree-proofs {
                                bounty-id: bounty-id,
                                planter: planter,
                            }
                                (merge proof {
                                    verification-count: new-count,
                                    verified: true,
                                })
                            )
                            (let ((bounty (unwrap! (map-get? bounties bounty-id)
                                    (err ERR-BOUNTY-NOT-FOUND)
                                )))
                                (map-set bounties bounty-id
                                    (merge bounty { trees-planted: (+ (get trees-planted bounty) u1) })
                                )
                                (var-set total-trees-planted
                                    (+ (var-get total-trees-planted) u1)
                                )
                                (ok true)
                            )
                        )
                        (ok true)
                    )
                )
                (ok true)
            )
        )
    )
)

(define-private (update-verifier-reputation
        (verifier principal)
        (correct-vote bool)
    )
    (let (
            (current-rep (get-user-reputation verifier))
            (new-total-verifications (+ (get total-verifications current-rep) u1))
            (new-correct-verifications (if correct-vote
                (+ (get correct-verifications current-rep) u1)
                (get correct-verifications current-rep)
            ))
            (accuracy-rate (if (> new-total-verifications u0)
                (/ (* new-correct-verifications u100) new-total-verifications)
                u50
            ))
            (reputation-adjustment (if (> accuracy-rate u75)
                u10
                (if (< accuracy-rate u25)
                    u20
                    u0
                )
            ))
            (new-score (if (> accuracy-rate u75)
                (let ((proposed-score (+ (get score current-rep) reputation-adjustment)))
                    (if (> proposed-score (var-get max-reputation))
                        (var-get max-reputation)
                        proposed-score
                    )
                )
                (if (< accuracy-rate u25)
                    (let ((proposed-score (- (get score current-rep) reputation-adjustment)))
                        (if (< proposed-score u10)
                            u10
                            proposed-score
                        )
                    )
                    (get score current-rep)
                )
            ))
        )
        (map-set user-reputation verifier {
            score: new-score,
            correct-verifications: new-correct-verifications,
            total-verifications: new-total-verifications,
            last-updated: burn-block-height,
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
            (merge proof {
                verified: true,
                verification-count: u1,
            })
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
