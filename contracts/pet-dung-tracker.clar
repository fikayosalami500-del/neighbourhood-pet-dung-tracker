;; Simple Dog Poop Bag Station
;; A neighborhood pet waste supply system with refill coordination

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_STATION_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_SUPPLY (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))

;; Station data structure
(define-map stations
  { station-id: uint }
  {
    location: (string-ascii 100),
    current-supply: uint,
    max-capacity: uint,
    last-refilled: uint,
    volunteer: (optional principal)
  }
)

;; Station counter
(define-data-var next-station-id uint u1)

;; Volunteer assignments
(define-map volunteer-schedules
  { volunteer: principal }
  {
    assigned-stations: (list 10 uint),
    next-refill-block: uint
  }
)

;; Add new station
(define-public (add-station (location (string-ascii 100)) (capacity uint))
  (let ((station-id (var-get next-station-id)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> capacity u0) ERR_INVALID_AMOUNT)
    (map-set stations
      { station-id: station-id }
      {
        location: location,
        current-supply: u0,
        max-capacity: capacity,
        last-refilled: stacks-block-height,
        volunteer: none
      }
    )
    (var-set next-station-id (+ station-id u1))
    (ok station-id)
  )
)

;; Refill station supply
(define-public (refill-station (station-id uint) (amount uint))
  (let ((station (unwrap! (map-get? stations { station-id: station-id }) ERR_STATION_NOT_FOUND)))
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= (+ (get current-supply station) amount) (get max-capacity station)) ERR_INVALID_AMOUNT)
    (map-set stations
      { station-id: station-id }
      (merge station {
        current-supply: (+ (get current-supply station) amount),
        last-refilled: stacks-block-height
      })
    )
    (ok (+ (get current-supply station) amount))
  )
)

;; Take bags from station
(define-public (take-bags (station-id uint) (amount uint))
  (let ((station (unwrap! (map-get? stations { station-id: station-id }) ERR_STATION_NOT_FOUND)))
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (get current-supply station) amount) ERR_INSUFFICIENT_SUPPLY)
    (map-set stations
      { station-id: station-id }
      (merge station {
        current-supply: (- (get current-supply station) amount)
      })
    )
    (ok (- (get current-supply station) amount))
  )
)

;; Assign volunteer to stations
(define-public (assign-volunteer (volunteer principal) (station-ids (list 10 uint)) (refill-interval uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set volunteer-schedules
      { volunteer: volunteer }
      {
        assigned-stations: station-ids,
        next-refill-block: (+ stacks-block-height refill-interval)
      }
    )
    ;; Update stations with volunteer assignment
    (fold update-station-volunteer station-ids volunteer)
    (ok true)
  )
)

;; Helper function to update station volunteer
(define-private (update-station-volunteer (station-id uint) (volunteer principal))
  (match (map-get? stations { station-id: station-id })
    station (begin
      (map-set stations
        { station-id: station-id }
        (merge station { volunteer: (some volunteer) })
      )
      volunteer
    )
    volunteer
  )
)

;; Get station info
(define-read-only (get-station-info (station-id uint))
  (map-get? stations { station-id: station-id })
)

;; Get volunteer schedule
(define-read-only (get-volunteer-schedule (volunteer principal))
  (map-get? volunteer-schedules { volunteer: volunteer })
)

;; Check if station needs refill (less than 25% capacity)
(define-read-only (needs-refill (station-id uint))
  (match (map-get? stations { station-id: station-id })
    station (< (get current-supply station) (/ (get max-capacity station) u4))
    false
  )
)

;; Get total stations count
(define-read-only (get-total-stations)
  (- (var-get next-station-id) u1)
)
