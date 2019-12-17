__includes [
  "model-breeds-own-globals.nls"
  "model-initialisation.nls"
  "policies.nls"
]


extensions [
  csv       ;; for reading and writing files
  profiler  ;; for efficient coding
  rnd       ;; for random selection from weighted lists
]


to setup

  print (word "Run number " behaviorspace-run-number)

  clear-all
  reset-ticks
  if(profile?)[
    profiler:reset         ;; clear the data
    profiler:start         ;; start profiling
  ]


  ; Read configuration file
  read-configuration

  ; Read energy prices
  read-energyprices

  ; Read defaults file
  ;read-defaults
  ; You should not do this at setup when using behavior space, it would override to defaults in all runs

  ; Read util coefficients
  read-util-functions

  ; Read and create technologies
  initialise-technologies

  ; Read stock values
  initialise-stock

  ; Read and create households
  initialise-households

  ; Create suppliers
  initialise-suppliers

  ; Create appliances
  initialise-appliances

  ; Energy stats at the start
  hh-consume-energy
end

to go

  ;Apply policies
  apply-policies


  ;Suppliers improve the appliances they sell
  suppl-update-technologies
  suppl-update-appliances


  ;Appliance break at the end of their lifetime
  appl-break-at-end-of-lifetime

  ;Appliance replacement for operating appliances
  appl-decide-on-replacement

  ;Households replace broken appliances (and those that they want to replace anyway)
  hh-replace-appliances

  ;Households use their appliances and consume energy by doing so
  hh-consume-energy

  ;Remove decommissioned appliances
  if remove-decommissioned-appls? [ ask appliances with [appl-status = "decommissioned"] [ die ] ]

  ;Update label in world
  ;ifelse ( show-old-decommissioned-devices? ) [ set label (word count appl-link-neighbors with [appl-status = "operating"] "(" count appl-link-neighbors ")    ") ] [ set label (word count appl-link-neighbors with [appl-status = "operating"] "    ") ]


  ;Record image interface
  if (record?) [
    export-interface (word "view" ticks ".png")
  ]

  ;Stop at end of simulation
  if ( ticks >= simulation-length-in-years * ticks-per-year ) [
    if(profile?)[
      profiler:stop          ;; stop profiling
      print profiler:report  ;; view the results
    ]
    stop
  ]

  ;Proceed to the next tick
  tick
end

to reset ;; create new case based on previously read data files
  reset-ticks
  clear-all-plots
  ask suppliers [
    ask appl-link-neighbors [
      die
    ]
  ]
  ask households [
    ask appl-link-neighbors [
      die
    ]
  ]
  ask appl-links [
    die
  ]
  initialise-appliances
end


to suppl-update-technologies
  ask technologies [
    ;Make white if not yet available
    ifelse ( tech-introduction-year <= ticks / ticks-per-year ) [
      set color item tech-label-index energy-label-colors ] [ set color white ]
  ]
  ;show (word ticks ": " count technologies)

  ;let countHatched 0
  ask technologies with [ ( ticks / ticks-per-year ) - tech-introduction-year = frequencyOfImprovedModels  ] [
    let yearsOnTheMarket ( ticks / ticks-per-year )  - tech-introduction-year
    ;show (word who " is hatching")
    ;set countHatched countHatched + 1
      hatch-technologies 1 [
        ;show (word who " was hatched")
        set tech-electricity-consumption tech-electricity-consumption * (1 + (tech-electricity-consumption-annual-change-percentage / 100)) ^ yearsOnTheMarket
        set tech-gas-consumption tech-gas-consumption * (1 + (tech-gas-consumption-annual-change-percentage / 100)) ^ yearsOnTheMarket
        set tech-introduction-year ticks / ticks-per-year
        hide-turtle
      ]
    ]
  ;show (word "hatched techs: " countHatched)
end

to suppl-update-appliances

  ;Remove old appliances from the shop
  ask appliances with [ appl-status = "for sale" and (([tech-introduction-year] of appl-tech) + maxAgeOfModelInStore) < (ticks / ticks-per-year) ] [ die ]


  ;Add new appliances based on old models
  ask suppliers [
    let TheSupplier self

    ;Add new appliances to the shop
    let possibleTechs technologies with [tech-introduction-year * ticks-per-year = ticks]
    let myTechs my-techs
    let countAdd 0
    if any? possibleTechs [
    ask possibleTechs  [ ; Look for technologies that are just new, consider one
      if not member? self myTechs and ((countAdd + count myTechs) < suppl-max-appls) [ ; only if not more than x products in store.
      set countAdd countAdd + 1
      let TheTech self
      ask TheSupplier [
        hatch-appliances 1 [
            create-appl-link-with TheSupplier [ hide-link ] ; link between supplier and stock item
            set appl-tech TheTech

            ;Apply defaults and details (colors, etc)
            apply-appliance-details-supplier
            ]
          ]
        ]
      ]
    ]

    ;Products on the market get cheaper over time (discount for older products)
    ask appl-link-neighbors [
      let yearsOnTheMarket ( ticks / ticks-per-year ) - [tech-introduction-year] of appl-tech
      set appl-price random-normal 1 .1 * [tech-price] of appl-tech * (1 + ([tech-price-annual-change-percentage] of appl-tech / 100)) ^ yearsOnTheMarket
    ]

  ]

  ;Remove appliances that are not anymore allowed
  ask suppliers [ removeApplsNotAllowed ]
end

; Remove appliances that with labels that are no longer allowed.
to removeApplsNotAllowed


    if labels-allowed-TV = "any label" []
    if labels-allowed-TV = "B or higher"           [ ask appl-link-neighbors with [ ( [ tech-type ] of appl-tech = "TV" ) and ( ( [ tech-label-index ] of appl-tech ) < position-energy-label "B" ) ] [ die ] ]
    if labels-allowed-TV = "A or higher"           [ ask appl-link-neighbors with [ ( [ tech-type ] of appl-tech = "TV" ) and ( ( [ tech-label-index ] of appl-tech ) < position-energy-label "A" ) ] [ die ] ]
    if labels-allowed-TV = "A+ or higher"          [ ask appl-link-neighbors with [ ( [ tech-type ] of appl-tech = "TV" ) and ( ( [ tech-label-index ] of appl-tech ) < position-energy-label "A+" ) ] [ die ] ]
    if labels-allowed-TV = "A++ or higher"         [ ask appl-link-neighbors with [ ( [ tech-type ] of appl-tech = "TV" ) and ( ( [ tech-label-index ] of appl-tech ) < position-energy-label "A++" ) ] [ die ] ]
    if labels-allowed-TV = "A+++ or higher"        [ ask appl-link-neighbors with [ ( [ tech-type ] of appl-tech = "TV" ) and ( ( [ tech-label-index ] of appl-tech ) < position-energy-label "A+++" ) ] [ die ] ]

    if labels-allowed-fridge = "any label" []
    if labels-allowed-fridge = "B or higher"           [ ask appl-link-neighbors with [ ( [ tech-type ] of appl-tech = "Fridge" ) and ( ( [ tech-label-index ] of appl-tech ) < position-energy-label "B" ) ] [ die ] ]
    if labels-allowed-fridge = "A or higher"           [ ask appl-link-neighbors with [ ( [ tech-type ] of appl-tech = "Fridge" ) and ( ( [ tech-label-index ] of appl-tech ) < position-energy-label "A" ) ] [ die ] ]
    if labels-allowed-fridge = "A+ or higher"          [ ask appl-link-neighbors with [ ( [ tech-type ] of appl-tech = "Fridge" ) and ( ( [ tech-label-index ] of appl-tech ) < position-energy-label "A+" ) ] [ die ] ]
    if labels-allowed-fridge = "A++ or higher"         [ ask appl-link-neighbors with [ ( [ tech-type ] of appl-tech = "Fridge" ) and ( ( [ tech-label-index ] of appl-tech ) < position-energy-label "A++" ) ] [ die ] ]
    if labels-allowed-fridge = "A+++ or higher"        [ ask appl-link-neighbors with [ ( [ tech-type ] of appl-tech = "Fridge" ) and ( ( [ tech-label-index ] of appl-tech ) < position-energy-label "A+++" ) ] [ die ] ]

    if labels-allowed-washmach = "any label" []
    if labels-allowed-washmach = "B or higher"           [ ask appl-link-neighbors with [ ( [ tech-type ] of appl-tech = "Washing Machine" ) and ( ( [ tech-label-index ] of appl-tech ) < position-energy-label "B" ) ] [ die ] ]
    if labels-allowed-washmach = "A or higher"           [ ask appl-link-neighbors with [ ( [ tech-type ] of appl-tech = "Washing Machine" ) and ( ( [ tech-label-index ] of appl-tech ) < position-energy-label "A" ) ] [ die ] ]
    if labels-allowed-washmach = "A+ or higher"          [ ask appl-link-neighbors with [ ( [ tech-type ] of appl-tech = "Washing Machine" ) and ( ( [ tech-label-index ] of appl-tech ) < position-energy-label "A+" ) ] [ die ] ]
    if labels-allowed-washmach  = "A++ or higher"         [ ask appl-link-neighbors with [ ( [ tech-type ] of appl-tech = "Washing Machine" ) and ( ( [ tech-label-index ] of appl-tech ) < position-energy-label "A++" ) ] [ die ] ]
    if labels-allowed-washmach  = "A+++ or higher"        [ ask appl-link-neighbors with [ ( [ tech-type ] of appl-tech = "Washing Machine" ) and ( ( [ tech-label-index ] of appl-tech ) < position-energy-label "A+++" ) ] [ die ] ]

    if labels-allowed-thermostat = "any label" []
    if labels-allowed-thermostat = "B or higher"           [ ask appl-link-neighbors with [ ( [ tech-type ] of appl-tech = "Thermostat" ) and ( ( [ tech-label-index ] of appl-tech ) < position-energy-label "B" ) ] [ die ] ]
    if labels-allowed-thermostat = "A or higher"           [ ask appl-link-neighbors with [ ( [ tech-type ] of appl-tech = "Thermostat" ) and ( ( [ tech-label-index ] of appl-tech ) < position-energy-label "A" ) ] [ die ] ]
    if labels-allowed-thermostat = "A+ or higher"          [ ask appl-link-neighbors with [ ( [ tech-type ] of appl-tech = "Thermostat" ) and ( ( [ tech-label-index ] of appl-tech ) < position-energy-label "A+" ) ] [ die ] ]
    if labels-allowed-thermostat = "A++ or higher"         [ ask appl-link-neighbors with [ ( [ tech-type ] of appl-tech = "Thermostat" ) and ( ( [ tech-label-index ] of appl-tech ) < position-energy-label "A++" ) ] [ die ] ]
    if labels-allowed-thermostat = "A+++ or higher"        [ ask appl-link-neighbors with [ ( [ tech-type ] of appl-tech = "Thermostat" ) and ( ( [ tech-label-index ] of appl-tech ) < position-energy-label "A+++" ) ] [ die ] ]

    if labels-allowed-heatingsystem = "any label" []
    if labels-allowed-heatingsystem = "B or higher"           [ ask appl-link-neighbors with [ ( [ tech-type ] of appl-tech = "Heating System" ) and ( ( [ tech-label-index ] of appl-tech ) < position-energy-label "B" ) ] [ die ] ]
    if labels-allowed-heatingsystem = "A or higher"           [ ask appl-link-neighbors with [ ( [ tech-type ] of appl-tech = "Heating System" ) and ( ( [ tech-label-index ] of appl-tech ) < position-energy-label "A" ) ] [ die ] ]
    if labels-allowed-heatingsystem = "A+ or higher"          [ ask appl-link-neighbors with [ ( [ tech-type ] of appl-tech = "Heating System" ) and ( ( [ tech-label-index ] of appl-tech ) < position-energy-label "A+" ) ] [ die ] ]
    if labels-allowed-heatingsystem = "A++ or higher"         [ ask appl-link-neighbors with [ ( [ tech-type ] of appl-tech = "Heating System" ) and ( ( [ tech-label-index ] of appl-tech ) < position-energy-label "A++" ) ] [ die ] ]
    if labels-allowed-heatingsystem = "A+++ or higher"        [ ask appl-link-neighbors with [ ( [ tech-type ] of appl-tech = "Heating System" ) and ( ( [ tech-label-index ] of appl-tech ) < position-energy-label "A+++" ) ] [ die ] ]

end
to appl-break-at-end-of-lifetime
  ask appliances with [ appl-status = "operating" and ticks >= appl-end-of-life] [
     set appl-status "broken" ;it is broken.
  ]
end

to appl-decide-on-replacement
  ask households [
    let TheHousehold self
    if random-float 1 > 0.99 [;TODO not random, but informed by survey
      if any? appl-link-neighbors with [ appl-status = "operating" ] [
        ask one-of appl-link-neighbors with [ appl-status = "operating" ] [
          set appl-status "replace" ;i want to replace this appliance
        ]
      ]
    ]
    ;Replace fridges when older than we thought we'd use it
    if is-number? hh-fridge-life [
      ask appl-link-neighbors with [ [tech-type] of appl-tech = "Fridge" and (ticks - appl-tick-installed) / ticks-per-year >= [hh-fridge-life] of myself ] [
        set appl-status "replace"
      ]
    ]
  ]

end

to hh-replace-appliances
  ask households [
    let TheHousehold self
    if any? appl-link-neighbors with [ appl-status = "broken" or appl-status = "replace" ] [

      let selfSupplier hh-supplier
      let selfSupplierType [suppl-type] of hh-supplier

      ;; SELECT SUPPLIERS >>
      let mySupplierOptions hh-supplier

      ; EXAMPLE: same supplier as previous, supplier of the same type as previous, or any supplier
      ;TODO make agent-specific selection of shops to visit
      if supplier-selection = "same" [ set mySupplierOptions hh-supplier ]
      if supplier-selection = "type" [ set mySupplierOptions (suppliers with [ suppl-type = selfSupplierType ])]
      if supplier-selection = "all"  [ set mySupplierOptions suppliers ]
      ;; SELECT SUPPLIERS <<

      ask appl-link-neighbors with [ appl-status = "broken" or appl-status = "replace" ] [
        let oldAppl self
        let selfEnergyLabel position ( [ tech-label ] of appl-tech ) all-energy-labels
        let selfTechType ( [ tech-type ] of appl-tech )

        ;; APPLIANCE OPTIONS >>
        ;; Get all appliances from the tech type from all shops I selected
        let myApplianceLonglist ( ( turtle-set ( [ appl-link-neighbors ] of mySupplierOptions ) ) with [ ( [ tech-type ] of appl-tech = selfTechType ) ] )

        ;TODO remove duplicates that you find in multiple shops, by taking the cheapest? Do the duplicates have too much priority with the current combination of alpha, price differentiation, etc?

        ;We select those that comply with our requirements/wishes.
        ;Our requirements are options with particular energy labels, prices, sizes, tech-type, number of options.
        if selfTechType = "Fridge" [
          ;we select on size (the outer size should be similar, within a margin, so it fits in the same space)
          let currentSize [tech-size] of appl-tech
          set myApplianceLonglist ( turtle-set ( myApplianceLonglist with [ ( abs([ tech-size ] of appl-tech - currentSize) < hh-fridge-size-range )] ) )
        ]

        if selfTechType = "TV" [
          ;we select on size, tv diagonal at least the size of the current TV
          let currentSize [tech-size] of appl-tech
          set myApplianceLonglist ( turtle-set ( myApplianceLonglist with [ ( [ tech-size ] of appl-tech >= currentSize) ] ) )
        ]

        let alreadySmartThermostat false
        if not can-abolish-smart-thermostat? [
        if selfTechType = "Thermostat" [
          ;Once you have a smart thermostat you don't go back.
          if [tech-label] of appl-tech = "A" [
            set alreadySmartThermostat true
            set myApplianceLonglist ( turtle-set ( myApplianceLonglist with [ ( [ tech-label ] of appl-tech = "A") ] ) )
          ]
        ]
        ]

        set myApplianceLonglist applyGeneralApplianceRestrictions TheHousehold selfTechType myApplianceLonglist

        ;Then we select a total of "number of options" from what they offer, or as many options as are available.
        let numberOfOptions min (list (count myApplianceLonglist) ([hh-number-of-options] of TheHousehold))
        let myApplianceShortlist n-of numberOfOptions myApplianceLonglist

        ;We consider devices that my neighbors/friends have by adding them to our short list if we had them on our long list
        ;Check how many friends and neigbors I could ask
        let friends-to-ask min (list number-of-friends-to-consider count friend-neighbors)
        let neighbors-to-ask min (list number-of-neighbors-to-consider count households in-radius neighbor-radius-to-consider )

        ;Ask friends and neighors
        let households-to-ask (turtle-set n-of friends-to-ask friend-neighbors n-of neighbors-to-ask households in-radius neighbor-radius-to-consider )
        let theirTechs turtle-set nobody
        ask households-to-ask [
          ; if the appliance that this household has appears on my long list, but not on my short list, have the chance of adding it to my short list.
          if any? appl-link-neighbors with [[tech-type] of appl-tech = selfTechType] [
            let hisTech [appl-tech] of one-of appl-link-neighbors with [[tech-type] of appl-tech = selfTechType]
              set theirTechs (turtle-set theirTechs hisTech)
          ]
        ]

        ;Add number of their appliances to my shortlist (if it was found)
        let n 1
        while [n <= number-of-friends-appls-to-short-list ] [
          ;TODO: we assume that neighbors like their own device
          let toAdd appl-for-tech one-of theirTechs myApplianceLongList
          if (toAdd != nobody) [
             ;We add it to the short list if it wasn't there
             set myApplianceShortlist (turtle-set myApplianceShortlist toAdd)
          ]
          set n n + 1
        ]

         if not can-abolish-smart-thermostat? [
        if selfTechType = "Thermostat" and alreadySmartThermostat [
          set myApplianceShortlist ( turtle-set ( myApplianceShortlist with [ ( [ tech-label ] of appl-tech = "A") ] ) )
        ]
        ]

        ;show (word "Appliance options considered: " count myApplianceOptions)
        ;; APPLIANCE SELECTION FROM OPTIONS >>
        ;show count myApplianceShortlist
        ifelse ( any? myApplianceShortlist ) [
          set appl-status "decommissioned"
          let selfShape shape
          let selfSize size

          ; Choose appliance from the options, on the basis of my utility function
          let applianceSubstitute chooseAppliance TheHousehold myApplianceShortlist

          let TheSeller [ one-of appl-link-neighbors ] of applianceSubstitute
          ask TheHousehold [ set hh-supplier TheSeller ] ; the one selling me this appliance is now my preferred supplier

          ; Order the new appliance
          hatch-appliances 1 [
            ; appliance has only one appl-link-neighbor (the household), so 'one-of' them is always the household:
            create-appl-link-with one-of ( [ appl-link-neighbors ] of myself ) [ hide-link ] ; link between household and appliance
            set appl-tech [ appl-tech ] of applianceSubstitute
            set appl-tick-installed ticks

            ; if old appliances are shown, make new once appear smaller.
            ifelse remove-decommissioned-appls? [ set size selfSize ] [ set size 0.75 * selfSize ]
            apply-appliance-details-household

            ; For heating systems, update the gas consumption
            if selfTechType = "Heating System" [
              let newEff [tech-label-efficiency] of appl-tech
              let oldTech [appl-tech] of oldAppl
              let oldEff [tech-label-efficiency] of oldTech
              ask TheHousehold [ set hh-gas-consumption-base hh-gas-consumption-base * oldEff / newEff ]
            ]

            ; For fridges, display old and new label
;            if selfTechType = "Fridge" [
;              let newLabel [tech-label] of appl-tech
;              let oldTech [appl-tech] of oldAppl
;              let oldLabel [tech-label] of oldTech
;              show (word "Old label: " oldLabel ", replaced by: " newLabel)
;            ]

            ;Pay for the appliance
            let price appl-price ; [ tech-price ] of appl-tech
            ask TheHousehold [ set hh-capital hh-capital - price ]
            ask TheHousehold [
              ask hh-supplier [
                set suppl-capital suppl-capital + price
                set suppl-sales suppl-sales + 1
              ]
            ]
          ]
        ]
        ;; APPLIANCE SELECTION FROM OPTIONS <<
        [ ;show (word "No options to replace my " selfTechType)
          ifelse ( decommission-old-appliances = "operating" ) [
            ; do nothing
          ]
          [
            set color brown
            set appl-status "old"
          ]
        ]
      ]
    ]
  ]

end

;These general restrictions apply to model initialization and purchasing new appliances
to-report applyGeneralApplianceRestrictions [ TheHousehold TechType myApplianceLonglist ]

  if TechType = "Washing Machine" [
    ;we select on size in a range around reasonable values for the household size
    let number min (list 3 [hh-size] of TheHousehold)

    ;TODO removed restriction.
    ;set myApplianceLonglist ( turtle-set ( myApplianceLonglist with [ ( abs([ tech-volume ] of appl-tech - hh-washmach-volume-per-hh-member * number ) <= hh-washmach-volume-range ) ] ) )

  ]

  report myApplianceLonglist
end

; For thermostats, at the beginning set a random smart or regular thermostat
to-report applyThermosatRestrictionsAtStart [ TechType myApplianceLonglist smart ]

  if TechType = "Thermostat" [
    ifelse smart = "Y" [ set myApplianceLonglist turtle-set ( myApplianceLonglist with [ [ tech-label ] of appl-tech = "A" ] ) ] ;I have a smart thermostat
                       [ set myApplianceLonglist turtle-set ( myApplianceLonglist with [ [ tech-label ] of appl-tech = "B" ] ) ] ;I have a regular thermostat
  ]
  report myApplianceLonglist
end


; For fridges, select those of a particular label
to-report applyLabelRestrictionsAtStart [ myApplianceLonglist the-label ]
  report turtle-set ( myApplianceLonglist with [ [ tech-label ] of appl-tech = the-label ] ) ;I have a smart thermostat
end


;Perspective: old appliance
to-report chooseAppliance [ the-household myApplianceOptions ]
  let subsidyForMe can-I-get-subsidy the-household
  let currentGasConsumption ( [ hh-heatcost ] of the-household ) / gas-price
  let currentHeatEfficiency 0
  let the-heating-system my-heatingsystem the-household
  if the-heating-system != nobody [
    set currentHeatEfficiency [ tech-label-efficiency ] of the-heating-system
  ]

  ask myApplianceOptions [
    set my-hh-util-e hh-util-e the-household self subsidyForMe currentGasConsumption currentHeatEfficiency
  ]
  report rnd:weighted-one-of myApplianceOptions [ my-hh-util-e ]
end



;Perspective: New appliance
to-report hh-util-e [ the-household the-appliance subsidyForMe currentGasConsumption currentHeatEfficiency ]
  ;myself = old appliance
  ;self = new appliance

  ; tech properties that go into the util func
  let the-tech-type [tech-type] of appl-tech
  let TechPrice appl-price ;[tech-price] of appl-tech
  let TechWarranty [tech-warranty] of appl-tech
  let rating [tech-rating] of appl-tech

  ; hh properties that go in to the util func
  let alpha     [hh-util-func-alpha] of the-household
  let bigfamily [hh-bigfamily] of the-household
  let lowinc    [hh-lowincome] of the-household
  let henvid    [hh-envbehav] of the-household
  let elder     [hh-elder] of the-household

  ;print (word the-tech-type " at price "  TechPrice)

  let subsidy 0
  if subsidyForMe [
    set subsidy apply-label-subsidy appl-tech
    set TechPrice TechPrice - subsidy
    ;print (word "subsidy " subsidy)
  ]

  let thelabel [tech-label] of appl-tech
  ;print (word the-tech-type " with label " thelabel " at price "  TechPrice " with subsidy " subsidy)

  let betas []
  let util 0

  if the-tech-type = "Fridge" [
    let TechA2 0
    if [tech-label] of appl-tech = "A++" [ set TechA2 1 ]

    let TechA3_0 0
    let TechA3_Sub 0
    if [tech-label] of appl-tech = "A+++" [ ifelse subsidy > 0 [ set TechA3_0 1] [ set TechA3_Sub 1 ] ]

    let TechStar4 0
    let TechStar45 0
    ifelse [tech-rating] of appl-tech >= 4.5 [ set TechStar45 1 ] [ if [tech-rating] of appl-tech >= 4 [ set TechStar4 1] ]

    let TechSize [tech-size] of appl-tech

    set betas fridge-util-coefficients
    set util util + (item 0 betas * TechPrice)
    set util util + (item 1 betas +  item 2 betas  * bigfamily + item 3 betas  * lowinc + item 4 betas  * henvid ) * TechSize
    set util util + (item 5 betas +  item 6 betas  * bigfamily + item 7 betas  * lowinc + item 8 betas  * henvid ) * TechWarranty
    set util util + (item 9 betas +  item 10 betas * bigfamily + item 11 betas * lowinc + item 12 betas * henvid ) * TechA2
    set util util + (item 13 betas + item 14 betas * bigfamily + item 15 betas * lowinc + item 16 betas * henvid ) * TechA3_0
    set util util + (item 17 betas + item 18 betas * bigfamily + item 19 betas * lowinc + item 20 betas * henvid ) * TechA3_sub
    set util util + (item 21 betas + item 22 betas * bigfamily + item 23 betas * lowinc + item 24 betas * henvid ) * TechStar4
    set util util + (item 25 betas + item 26 betas * bigfamily + item 27 betas * lowinc + item 28 betas * henvid ) * TechStar45
  ]

  if the-tech-type = "Washing Machine" [
    set betas washmach-util-coefficients
    set util util + (item 0 betas * TechPrice)
  ]

  if the-tech-type = "TV" [
    set betas tv-util-coefficients
    set util util + (item 0 betas * TechPrice)
  ]

  if the-tech-type = "Thermostat" [

    let Savings [tech-savings] of appl-tech
    let HasRemote [tech-remote] of appl-tech
    let HasDisplay [tech-display] of appl-tech
    let RecommendedProvider 0 ;TODO
    let RecommendedExpert 0  ;TODO

    set betas thermostat-util-coefficients
    set util util + (item 0 betas * TechPrice)
    set util util + (item 1 betas +  item 2 betas  * elder + item 3 betas  * lowinc  ) * subsidy
    set util util + (item 4 betas +  item 5 betas  * elder + item 6 betas  * lowinc  ) * Savings
    set util util + (item 7 betas +  item 8 betas  * elder + item 9 betas  * lowinc  ) * RecommendedProvider
    set util util + (item 10 betas + item 11 betas * elder + item 12 betas * lowinc  ) * RecommendedExpert
    set util util + (item 13 betas + item 14 betas * elder + item 15 betas * lowinc  ) * HasRemote
    set util util + (item 16 betas + item 17 betas * elder + item 18 betas * lowinc  ) * HasDisplay
  ]

  if the-tech-type = "Heating System" [
    set betas heatingsystem-util-coefficients
    let Savings ( currentGasConsumption - currentGasConsumption * currentHeatEfficiency / ( [ tech-label-efficiency ] of appl-tech ) ) * gas-price ; IB190325
    ;show (word "gas cost " (currentGasConsumption * gas-price))
    ;show (word "current efficiency " currentHeatEfficiency)
    ;show (word "new efficiency " [ tech-label-efficiency ] of appl-tech)
    ;show (word "savings " Savings)
    let SubsidyPublic 0
    if who-gives-subsidy = "public" [set SubsidyPublic subsidy ]
    let SubsidyPrivate 0
    if who-gives-subsidy = "private" [set SubsidyPrivate subsidy ]

    let InstallTime [tech-install-time] of appl-tech

    set util util + (item 0 betas * TechPrice)
    set util util + (item 1 betas +  item 2 betas  * elder + item 3 betas  * lowinc  ) * SubsidyPublic
    set util util + (item 4 betas +  item 5 betas  * elder + item 6 betas  * lowinc  ) * SubsidyPrivate
    set util util + (item 7 betas +  item 8 betas  * elder + item 9 betas  * lowinc  ) * Savings
    set util util + (item 10 betas + item 11 betas * elder + item 12 betas * lowinc  ) * InstallTime
    set util util + (item 13 betas + item 14 betas * elder + item 15 betas * lowinc  ) * TechWarranty
  ]

  if empty? betas [ show word "Utility function: no tech match found - " the-tech-type ]
  if util > 700 [ set util 700 ]
  report e ^ ( alpha * util )

end


;; Determine how much energy is consumed in this tick and record the costs
to hh-consume-energy
  ask households [
    let electricityInTick 0
    let gasInTick 0
    ask appl-link-neighbors with [ appl-status = "operating" ] [
      set electricityInTick electricityInTick + [ tech-electricity-consumption] of appl-tech
      ;set gasInTick gasInTick + [ tech-gas-consumption] of appl-tech ; TODO here specific to house? Add savings effect of smart thermostat!
    ]
    set gasInTick hh-gas-consumption-base
    let savings [tech-savings] of one-of my-techs with [tech-type = "Thermostat"]
    set gasInTick gasInTick * (100 - savings) / 100 ;lowers heat consumption for smart thermostats

    set hh-electricity-consumption electricityInTick
    set hh-gas-consumption gasInTick
    set hh-energy-cost hh-electricity-consumption * electricity-price + hh-gas-consumption * gas-price
  ]
end

;; Reporter to retrieve the position of an energy label
to-report position-energy-label [ label-string ]
  report position label-string all-energy-labels
end

to-report count-my-appliances-of-type [ the-appl-type ]
  report count ([appl-tech] of appl-link-neighbors) with [tech-type = the-appl-type]
end


to-report my-techs
  report (turtle-set [appl-tech] of appl-link-neighbors)
end

to-report appls-to-techs [appls]
  report (turtle-set [appl-tech] of appls)
end

to-report appl-for-tech [ TheTech TheAppls ]
  report one-of TheAppls with [appl-tech = TheTech]
end

to-report my-heatingsystem [ the-household ]
  let the-one nobody
  ask the-household [
    if any? my-techs with [tech-type = "Heating System"] [ set the-one one-of my-techs with [tech-type = "Heating System"] ]
  ]
  report the-one
end

to-report operating-appliances-with-label [ the-label ]
  report appliances with [ ( [ tech-label ] of appl-tech = the-label ) and ( appl-status = "operating" ) ]
end

to-report operating-appliances-of-type [ the-type ]
  report appliances with [ ( [ tech-type ] of appl-tech = the-type ) and ( appl-status = "operating" ) ]
end

to-report operating-appliances-sold-of-type [ the-type ]
  report appliances with [ ( [ tech-type ] of appl-tech = the-type )  and ( appl-status = "operating" ) and ((ticks - appl-tick-installed) / ticks-per-year) <= 1]
end

to-report operating-appliances-sold-with-label-of-type [ the-label the-type ]
  report appliances with [ ( [ tech-type ] of appl-tech = the-type ) and ( [ tech-label ] of appl-tech = the-label )  and ( appl-status = "operating" ) and ((ticks - appl-tick-installed) / ticks-per-year) <= 1]
end

to-report operating-appliances-with-label-of-type [ the-label the-type ]
  report appliances with [ ( [ tech-type ] of appl-tech = the-type ) and ( [ tech-label ] of appl-tech = the-label ) and ( appl-status = "operating" ) ]
end

to-report frac-appl-label-type [ the-label the-type ]
  if count operating-appliances-of-type the-type > 0 [
    report (count operating-appliances-with-label-of-type the-label the-type ) / ( count operating-appliances-of-type the-type )
  ]
  report 0
end

to-report frac-appl-sold-label-type [ the-label the-type ]
  if count operating-appliances-sold-of-type the-type > 0 [
    report (count operating-appliances-sold-with-label-of-type the-label the-type ) / ( count operating-appliances-sold-of-type the-type )
  ]
  report 0
end

to-report avg-gas-cons-hh-heating
  if any? households with [hh-have-heating] [
    report mean [hh-gas-consumption] of households with [hh-have-heating]
  ]
  report 0
end

to-report avg-el-cons-hh-fridge
  let c 0
  let s 0
  ask operating-appliances-of-type "Fridge" [
    set c c + 1
    set s s + [tech-electricity-consumption] of appl-tech
  ]
  ifelse (c > 0) [ report s / c ] [ report 0 ]
end


to-report avg-el-cons-hh-fridge-label [ the-label ]
  let c 0
  let s 0
  ask operating-appliances-with-label-of-type the-label "Fridge" [
    set c c + 1
    set s s + [tech-electricity-consumption] of appl-tech
  ]
  ifelse (c > 0) [ report s / c ] [ report 0 ]
end

to-report avg-el-cons-hh-fridge-sold
  let c 0
  let s 0
  ask operating-appliances-sold-of-type "Fridge" [
    set c c + 1
    set s s + [tech-electricity-consumption] of appl-tech
  ]
    ifelse (c > 0) [ report s / c ] [ report 0 ]
end


to-report avg-el-cons-hh-fridge-label-sold [ the-label ]
  let c 0
  let s 0
  ask operating-appliances-sold-with-label-of-type the-label "Fridge" [
    set c c + 1
    set s s + [tech-electricity-consumption] of appl-tech
  ]
    ifelse (c > 0) [ report s / c ] [ report 0 ]
end

to-report old-appliances
  report appliances with [ appl-status = "old" ]
end

to-report electricity-price
  ifelse length electricity-prices > ticks / ticks-per-year [
    report item (ticks / ticks-per-year) electricity-prices
  ][
    report last electricity-prices
  ]
end

to-report gas-price
  ifelse length gas-prices > ticks / ticks-per-year [
    report item (ticks / ticks-per-year) gas-prices
  ][
    report last gas-prices
  ]

end


to-report electricity-consumption-of-my-tech [ TechType ]
  report first [tech-electricity-consumption] of my-techs with [tech-type = TechType ]
end

to-report gas-consumption-of-my-tech [ TechType ]
  report first [tech-gas-consumption] of my-techs with [tech-type = TechType ]
end


to-report frac-low-income-operating-appliances-with-label-of-type [ the-label the-type ]
  let thenumber 0
  ask low-income-hh [
    ask appl-link-neighbors [
    if ( [ tech-type ] of appl-tech = the-type ) and ( [ tech-label ] of appl-tech = the-label ) and ( appl-status = "operating" ) [
      set thenumber thenumber + 1
    ]
  ]
  ]
  report thenumber / count low-income-hh
end

to-report frac-high-income-operating-appliances-with-label-of-type [ the-label the-type ]
  let thenumber 0
  ask high-income-hh [
    ask appl-link-neighbors [
    if ( [ tech-type ] of appl-tech = the-type ) and ( [ tech-label ] of appl-tech = the-label ) and ( appl-status = "operating" ) [
      set thenumber thenumber + 1
    ]
  ]
  ]
  report thenumber / count high-income-hh
end

to-report low-income-electricity-consumption-per-type [ the-type ]
  let thenumber 0
  ask low-income-hh [
    ask appl-link-neighbors [
    if ( [ tech-type ] of appl-tech = the-type ) and ( appl-status = "operating" ) [
        set thenumber thenumber + [tech-electricity-consumption] of appl-tech
    ]
  ]
  ]
  report thenumber / count low-income-hh
end

to-report high-income-electricity-consumption-per-type [ the-type ]
  let thenumber 0
  ask high-income-hh [
    ask appl-link-neighbors [
    if ( [ tech-type ] of appl-tech = the-type ) and ( appl-status = "operating" ) [
        set thenumber thenumber + [tech-electricity-consumption] of appl-tech
    ]
  ]
  ]
  report thenumber / count high-income-hh
end


to-report low-income-hh
  report households with [hh-lowincome = 1]
end

to-report high-income-hh
  report households with [hh-lowincome = 0]
end


to-report frac-therm-smart-remote
  report (count (operating-appliances-with-label-of-type "A" "Thermostat") with [ ( [ tech-remote ] of appl-tech = 1 ) ] ) / ( count operating-appliances-of-type "Thermostat" )
end

to-report frac-therm-sold-smart-remote
  report (count (operating-appliances-sold-with-label-of-type "A" "Thermostat") with [ ( [ tech-remote ] of appl-tech = 1 ) ] ) / ( count operating-appliances-of-type "Thermostat" )
end

to-report frac-therm-smart-display
  report (count (operating-appliances-with-label-of-type "A" "Thermostat") with [ ( [ tech-display ] of appl-tech = 1 ) ] ) / ( count operating-appliances-of-type "Thermostat" )
end

to-report frac-therm-sold-smart-display
  report (count (operating-appliances-sold-with-label-of-type "A" "Thermostat") with [ ( [ tech-display ] of appl-tech = 1 ) ] ) / ( count operating-appliances-of-type "Thermostat" )
end
@#$#@#$#@
GRAPHICS-WINDOW
606
10
1193
598
-1
-1
4.79
1
10
1
1
1
0
0
0
1
0
120
0
120
1
1
1
ticks
30.0

BUTTON
8
10
71
43
setup
setup
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

BUTTON
8
115
71
148
NIL
go
T
1
T
OBSERVER
NIL
G
NIL
NIL
0

BUTTON
8
80
71
113
step
go
NIL
1
T
OBSERVER
NIL
Q
NIL
NIL
0

PLOT
5
367
443
578
Energy labels of appliances
Tick
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"C" 1.0 0 -11053225 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label \"C\" ]"
"B" 1.0 0 -7500403 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label \"B\" ] "
"A" 1.0 0 -4539718 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label \"A\" ]"
"A+" 1.0 0 -2674135 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label \"A+\" ]"
"A++" 1.0 0 -5509967 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label \"A++\" ]"
"A+++" 1.0 0 -1184463 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label \"A+++\" ]"
"Old" 1.0 0 -8431303 true "" "if plot? and ticks != 0 [ plot count old-appliances ]"
"D" 1.0 0 -16777216 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label \"D\" ]"

PLOT
279
810
514
1019
Energy labels of TV's
Tick
Number of operating TV's
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"C" 1.0 0 -11053225 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label-of-type \"C\" \"TV\" ]"
"B" 1.0 0 -7500403 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label-of-type \"B\" \"TV\" ]"
"A" 1.0 0 -4539718 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label-of-type \"A\" \"TV\" ]"
"A+" 1.0 0 -2674135 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label-of-type \"A+\" \"TV\" ]"
"A++" 1.0 0 -5509967 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label-of-type \"A++\" \"TV\" ]"
"A+++" 1.0 0 -1184463 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label-of-type \"A+++\" \"TV\" ]"
"Old" 1.0 0 -8431303 true "" "if plot? and ticks != 0 [ plot count old-appliances with [ ( [ tech-type ] of appl-tech = \"TV\" ) ] ]"

PLOT
7
1033
273
1245
Energy labels of Washing machines
Tick
Number of operating washers
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"C" 1.0 0 -11053225 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label-of-type \"C\" \"Washing Machine\" ]"
"B" 1.0 0 -7500403 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label-of-type \"B\" \"Washing Machine\" ]"
"A" 1.0 0 -4539718 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label-of-type \"A\" \"Washing Machine\" ]"
"A+" 1.0 0 -2674135 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label-of-type \"A+\" \"Washing Machine\" ]"
"A++" 1.0 0 -5509967 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label-of-type \"A++\" \"Washing Machine\" ]"
"A+++" 1.0 0 -1184463 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label-of-type \"A+++\" \"Washing Machine\" ]"
"Old" 1.0 0 -8431303 true "" "if plot? and ticks != 0 [ plot count old-appliances with [ ( [ tech-type ] of appl-tech = \"Washing Machine\" ) ] ]"

PLOT
277
1034
516
1245
Energy labels of Fridges
Tick
Number of operating fridges
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"C" 1.0 0 -11053225 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label-of-type \"C\" \"Fridge\" ]"
"B" 1.0 0 -7500403 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label-of-type \"B\" \"Fridge\" ]"
"A" 1.0 0 -4539718 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label-of-type \"A\" \"Fridge\" ]"
"A+" 1.0 0 -2674135 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label-of-type \"A+\" \"Fridge\" ]"
"A++" 1.0 0 -5509967 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label-of-type \"A++\" \"Fridge\" ]"
"A+++" 1.0 0 -1184463 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label-of-type \"A+++\" \"Fridge\" ]"
"Old" 1.0 0 -8431303 true "" "if plot? and ticks != 0 [ plot count old-appliances with [ ( [ tech-type ] of appl-tech = \"Fridge\" ) ] ]"

MONITOR
1782
35
1979
80
NIL
count appliances
17
1
11

MONITOR
1782
178
1978
223
operating appliances
count appliances with [ appl-status = \"operating\" ]
17
1
11

CHOOSER
1198
136
1422
181
supplier-selection
supplier-selection
"same" "type" "all"
2

CHOOSER
1785
516
2003
561
decommission-old-appliances
decommission-old-appliances
"operating" "decommission"
1

MONITOR
1782
82
1978
127
decommissioned appliances
count appliances with [ appl-status = \"decommissioned\" ]
17
1
11

SLIDER
85
148
243
181
suppl-max-appls
suppl-max-appls
1
500
150.0
1
1
NIL
HORIZONTAL

BUTTON
8
45
71
78
NIL
reset
NIL
1
T
OBSERVER
NIL
R
NIL
NIL
0

MONITOR
1782
321
1988
366
Average age of appliances (year)
(ticks - mean [appl-tick-installed] of appliances with [appl-status = \"operating\"] ) / ticks-per-year
2
1
11

MONITOR
1782
226
1978
271
broken appliances
count appliances with [appl-status = \"broken\"]
17
1
11

CHOOSER
448
31
598
76
labels-allowed-TV
labels-allowed-TV
"any label" "B or higher" "A or higher" "A+ or higher" "A++ or higher" "A+++ or higher"
0

PLOT
4
582
273
799
Replacement
Tick
Number of replaced appliances
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if plot? [plot count appliances with [ appl-end-of-life = ticks]]"

MONITOR
1782
130
1976
175
appliances with suppliers
sum [ count in-link-neighbors ] of suppliers
17
1
11

PLOT
538
624
815
774
Money of households
Tick
Money (euro)
-5000.0
40000.0
0.0
10.0
true
true
"" ""
PENS
"households" 50.0 1 -16777216 true "" "if plot? [histogram [hh-capital] of households]"
"suppliers" 1.0 0 -7500403 true "" "if plot? [histogram [suppl-capital] of suppliers]"

PLOT
538
1086
815
1236
Supplier money
Tick
Money (Euro)
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"ESCO" 1.0 0 -16777216 true "" "if plot? and ticks != 0 [plot mean [suppl-capital] of (suppliers with [suppl-type = \"ESCO\"])]"
"Shop" 1.0 0 -7500403 true "" "if plot? and ticks != 0 [plot mean [suppl-capital] of (suppliers with [suppl-type = \"Shop\"])]"
"Webshop" 1.0 0 -2674135 true "" "if plot? and ticks != 0 [plot mean [suppl-capital] of (suppliers with [suppl-type = \"Webshop\"])]"

PLOT
538
932
815
1082
Supplier sales
NIL
NIL
0.0
30.0
0.0
10.0
true
true
"" ""
PENS
"ESCO" 1.0 0 -13791810 true "" "if plot? and ticks != 0 [plot sum [suppl-sales] of (suppliers with [suppl-type = \"ESCO\"])]"
"Shop" 1.0 0 -10899396 true "" "if plot? and ticks != 0 [plot sum [suppl-sales] of (suppliers with [suppl-type = \"Shop\"])]"
"Webshop" 1.0 0 -2674135 true "" "if plot? and ticks != 0 [plot first [suppl-sales] of (suppliers with [suppl-type = \"Webshop\"])]"

PLOT
824
624
1101
774
Global average energy use
Tick
kWh or m3/year
0.0
30.0
0.0
10.0
true
true
"" ""
PENS
"electricity fridge" 1.0 0 -16777216 true "" "if plot? and ticks != 0 [ plot avg-el-cons-hh-fridge ]"
"gas heating" 1.0 0 -7500403 true "" "if plot? and ticks != 0 [ plot avg-gas-cons-hh-heating ]"

PLOT
538
778
815
928
Household money
Tick
Money (euro)
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if plot? and ticks != 0 [ plot mean [hh-capital] of households]"

SWITCH
1784
477
2000
510
show-old-decommissioned-devices?
show-old-decommissioned-devices?
1
1
-1000

PLOT
823
932
1100
1082
Average Energy costs
Tick
Costs (euro/year)
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if plot? and ticks != 0 [plot mean [hh-energy-cost] of households]"

SWITCH
1787
562
1890
595
profile?
profile?
1
1
-1000

SWITCH
1789
602
1892
635
record?
record?
1
1
-1000

SWITCH
1899
604
2002
637
plot?
plot?
0
1
-1000

CHOOSER
85
31
243
76
country
country
"Default" "France" "Germany" "Italy" "Poland" "Romania" "Spain" "Sweden" "UK"
0

MONITOR
1782
273
1978
318
replacing
count appliances with [appl-status = \"replace\"]
0
1
11

PLOT
1404
219
1762
378
Friends degree distribution
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "if plot? and ticks != 0 [\nlet max-degree max [count my-friends] of households\nplot-pen-reset  ;; erase what we plotted before\nset-plot-x-range 1 (max-degree + 1)  ;; + 1 to make room for the width of the last bar\nhistogram [count my-friends] of households\n]"

SLIDER
1388
979
1778
1012
tech-price-annual-change-percentage-TV
tech-price-annual-change-percentage-TV
-5
5
0.0
.1
1
NIL
HORIZONTAL

SLIDER
1388
939
1778
972
tech-price-annual-change-percentage-fridge
tech-price-annual-change-percentage-fridge
-5
5
-3.0
.1
1
NIL
HORIZONTAL

SLIDER
1388
899
1778
932
tech-price-annual-change-percentage-washmach
tech-price-annual-change-percentage-washmach
-5
5
0.0
.1
1
NIL
HORIZONTAL

SLIDER
1388
859
1778
892
tech-price-annual-change-percentage-thermostat
tech-price-annual-change-percentage-thermostat
-5
5
-3.0
.1
1
NIL
HORIZONTAL

SLIDER
1388
819
1778
852
tech-price-annual-change-percentage-heatingsystem
tech-price-annual-change-percentage-heatingsystem
-5
5
-3.0
.1
1
NIL
HORIZONTAL

SLIDER
1388
659
1778
692
tech-electricity-consumption-annual-change-percentage-TV
tech-electricity-consumption-annual-change-percentage-TV
-5
5
0.0
.1
1
NIL
HORIZONTAL

SLIDER
1388
619
1778
652
tech-electricity-consumption-annual-change-percentage-fridge
tech-electricity-consumption-annual-change-percentage-fridge
-5
5
-1.3
.1
1
NIL
HORIZONTAL

SLIDER
1388
779
1778
812
tech-electricity-consumption-annual-change-percentage-washmach
tech-electricity-consumption-annual-change-percentage-washmach
-5
5
0.0
.1
1
NIL
HORIZONTAL

SLIDER
1388
699
1778
732
tech-electricity-consumption-annual-change-percentage-thermostat
tech-electricity-consumption-annual-change-percentage-thermostat
-5
5
0.0
.1
1
NIL
HORIZONTAL

SLIDER
1388
739
1778
772
tech-electricity-consumption-annual-change-percentage-heatingsystem
tech-electricity-consumption-annual-change-percentage-heatingsystem
-5
5
0.0
.1
1
NIL
HORIZONTAL

SLIDER
1388
539
1778
572
tech-gas-consumption-annual-change-percentage-TV
tech-gas-consumption-annual-change-percentage-TV
-5
5
0.0
.1
1
NIL
HORIZONTAL

SLIDER
1388
499
1778
532
tech-gas-consumption-annual-change-percentage-fridge
tech-gas-consumption-annual-change-percentage-fridge
-5
5
0.0
.1
1
NIL
HORIZONTAL

SLIDER
1388
459
1776
492
tech-gas-consumption-annual-change-percentage-washmach
tech-gas-consumption-annual-change-percentage-washmach
-5
5
0.0
.1
1
NIL
HORIZONTAL

SLIDER
1388
579
1778
612
tech-gas-consumption-annual-change-percentage-thermostat
tech-gas-consumption-annual-change-percentage-thermostat
-5
5
0.0
.1
1
NIL
HORIZONTAL

SLIDER
1388
417
1775
450
tech-gas-consumption-annual-change-percentage-heatingsystem
tech-gas-consumption-annual-change-percentage-heatingsystem
-5
5
-1.0
.1
1
NIL
HORIZONTAL

SLIDER
1428
66
1646
99
number-of-friends-to-consider
number-of-friends-to-consider
0
20
5.0
1
1
NIL
HORIZONTAL

SLIDER
1428
101
1646
134
number-of-neighbors-to-consider
number-of-neighbors-to-consider
0
20
5.0
1
1
NIL
HORIZONTAL

SLIDER
1428
136
1646
169
neighbor-radius-to-consider
neighbor-radius-to-consider
0
10
2.0
1
1
NIL
HORIZONTAL

SLIDER
1428
171
1646
204
number-of-friends-appls-to-short-list
number-of-friends-appls-to-short-list
0
20
3.0
1
1
NIL
HORIZONTAL

SLIDER
85
78
243
111
ticks-per-year
ticks-per-year
1
365
1.0
1
1
NIL
HORIZONTAL

SLIDER
85
113
243
146
simulation-length-in-years
simulation-length-in-years
1
40
20.0
1
1
NIL
HORIZONTAL

SLIDER
1198
31
1422
64
hh-fridge-size-range
hh-fridge-size-range
0
150
25.0
5
1
NIL
HORIZONTAL

SLIDER
1198
66
1422
99
hh-washmach-volume-per-hh-member
hh-washmach-volume-per-hh-member
1
5
2.5
.1
1
NIL
HORIZONTAL

SLIDER
1198
101
1422
134
hh-washmach-volume-range
hh-washmach-volume-range
0
5
3.0
.5
1
NIL
HORIZONTAL

SLIDER
1428
31
1646
64
min-number-of-friends
min-number-of-friends
1
10
3.0
1
1
NIL
HORIZONTAL

TEXTBOX
1430
11
1581
29
Agent interactions
13
0.0
1

BUTTON
8
150
71
183
defaults
read-defaults
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
1200
11
1396
43
Decision details / parameters
13
0.0
1

TEXTBOX
1775
10
1970
42
General statistics and debugging
13
0.0
1

TEXTBOX
88
10
238
28
General parameters
13
0.0
1

TEXTBOX
257
10
407
28
Policies
13
0.0
1

CHOOSER
448
78
598
123
labels-allowed-fridge
labels-allowed-fridge
"any label" "B or higher" "A or higher" "A+ or higher" "A++ or higher" "A+++ or higher"
3

CHOOSER
448
125
598
170
labels-allowed-washmach
labels-allowed-washmach
"any label" "B or higher" "A or higher" "A+ or higher" "A++ or higher" "A+++ or higher"
0

CHOOSER
254
78
443
123
policy-allowed-labels-fridge
policy-allowed-labels-fridge
"off" "slow" "fast" "fixed"
1

CHOOSER
254
31
443
76
policy-allowed-labels-tv
policy-allowed-labels-tv
"off" "slow" "fast" "fixed"
0

CHOOSER
254
125
443
170
policy-allowed-labels-washmach
policy-allowed-labels-washmach
"off" "slow" "fast" "fixed"
0

SWITCH
1899
565
2005
598
network?
network?
0
1
-1000

MONITOR
1784
376
1901
421
NIL
count technologies
17
1
11

PLOT
823
779
1100
928
Energy prices
Tick
Price (euro/kWh)
0.0
10.0
0.0
0.25
true
true
"" ""
PENS
"electricity" 1.0 0 -16777216 true "" "if ticks != 0 [ plot electricity-price ]"
"gas" 1.0 0 -7500403 true "" "if ticks != 0 [ plot gas-price ]"

CHOOSER
448
219
598
264
labels-allowed-heatingsystem
labels-allowed-heatingsystem
"any label" "B or higher" "A or higher" "A+ or higher" "A++ or higher" "A+++ or higher"
0

CHOOSER
448
172
598
217
labels-allowed-thermostat
labels-allowed-thermostat
"any label" "B or higher" "A or higher" "A+ or higher" "A++ or higher" "A+++ or higher"
0

PLOT
276
582
512
799
Energy labels of Thermostats
Tick
Number of operating thermostats
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Regular" 1.0 0 -16448764 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label-of-type \"B\" \"Thermostat\" ]"
"Smart" 1.0 0 -13840069 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label-of-type \"A\" \"Thermostat\" ]"

PLOT
10
811
268
1021
Energy labels of Heating Sytems
Tick
Number of operating heating Systems
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"C" 1.0 0 -12895429 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label-of-type \"C\" \"Heating System\" ]"
"B" 1.0 0 -7500403 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label-of-type \"B\" \"Heating System\" ]"
"A" 1.0 0 -4539718 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label-of-type \"A\" \"Heating System\" ]"
"A+" 1.0 0 -2674135 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label-of-type \"A+\" \"Heating System\" ]"
"A++" 1.0 0 -5509967 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label-of-type \"A++\" \"Heating System\" ]"
"A+++" 1.0 0 -1184463 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label-of-type \"A+++\" \"Heating System\" ]"
"Old" 1.0 0 -6459832 true "" "if plot? and ticks != 0 [ plot count old-appliances with [ ( [ tech-type ] of appl-tech = \"Heating System\" ) ] ]"
"D" 1.0 0 -16777216 true "" "if plot? and ticks != 0 [ plot count operating-appliances-with-label-of-type \"D\" \"Heating System\" ]"

SWITCH
1781
441
2002
474
remove-decommissioned-appls?
remove-decommissioned-appls?
0
1
-1000

SLIDER
254
269
443
302
first-year-subsidy
first-year-subsidy
0
40
4.0
1
1
NIL
HORIZONTAL

SLIDER
254
304
443
337
last-year-subsidy
last-year-subsidy
0
40
20.0
1
1
NIL
HORIZONTAL

TEXTBOX
14
340
164
358
Labels
13
0.0
1

TEXTBOX
542
602
692
620
Money
13
0.0
1

TEXTBOX
826
602
976
620
Energy
13
0.0
1

CHOOSER
448
363
598
408
minimum-label-subsidy
minimum-label-subsidy
7 6 5 4 3 2 1
6

TEXTBOX
1397
387
1547
405
Technology learning
13
0.0
1

CHOOSER
254
172
443
217
policy-allowed-labels-heatingsystem
policy-allowed-labels-heatingsystem
"off" "slow" "fast" "fixed"
0

CHOOSER
254
219
443
264
policy-allowed-labels-thermostat
policy-allowed-labels-thermostat
"off" "slow" "fast" "fixed"
0

INPUTBOX
1198
443
1339
503
subsidylevel-fridge
0.0
1
0
Number

INPUTBOX
1198
381
1339
441
subsidylevel-wasmach
0.0
1
0
Number

INPUTBOX
1198
257
1339
317
subsidylevel-heatingsystem
0.0
1
0
Number

INPUTBOX
1198
195
1339
255
subsidylevel-TV
0.0
1
0
Number

INPUTBOX
1198
319
1339
379
subsidylevel-thermostat
0.0
1
0
Number

SLIDER
1788
644
1960
677
DeveloperMode
DeveloperMode
0
500
0.0
100
1
NIL
HORIZONTAL

CHOOSER
448
316
598
361
Who-to-subsidies
Who-to-subsidies
"low-income" "all" "nobody"
0

CHOOSER
448
269
598
314
Who-gives-subsidy
Who-gives-subsidy
"Public" "Private"
0

SLIDER
85
194
243
227
global-alpha
global-alpha
0
10
1.0
1
1
NIL
HORIZONTAL

SLIDER
85
229
243
262
percentageSmartThermostatLowersGas
percentageSmartThermostatLowersGas
0
100
5.0
1
1
NIL
HORIZONTAL

SLIDER
85
264
243
297
frequencyOfImprovedModels
frequencyOfImprovedModels
1
10
1.0
1
1
NIL
HORIZONTAL

SLIDER
85
299
243
332
maxAgeOfModelInStore
maxAgeOfModelInStore
0
10
4.0
1
1
NIL
HORIZONTAL

PLOT
1110
727
1310
877
Electricity consumption fridges
tick
kWh/year
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"sold" 1.0 0 -16777216 true "" "if plot? and ticks != 0 [ plot avg-el-cons-hh-fridge-sold ]"
"all" 1.0 0 -7500403 true "" "if plot? and ticks != 0 [ plot avg-el-cons-hh-fridge ]"
"A+++ sold" 1.0 0 -2674135 true "" "if plot? and ticks != 0 [ plot avg-el-cons-hh-fridge-label-sold \"A+++\" ]"
"A+++ all" 1.0 0 -955883 true "" "if plot? and ticks != 0 [ plot avg-el-cons-hh-fridge-label \"A+++\" ]"

SWITCH
1196
511
1384
544
can-abolish-smart-thermostat?
can-abolish-smart-thermostat?
0
1
-1000

SWITCH
1197
549
1384
582
remove-hh-without-heating-cost?
remove-hh-without-heating-cost?
1
1
-1000

INPUTBOX
1197
588
1346
648
util-function-uncertainty
0.0
1
0
Number

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

building store
false
0
Rectangle -7500403 true true 30 45 45 240
Rectangle -16777216 false false 30 45 45 165
Rectangle -7500403 true true 15 165 285 255
Rectangle -16777216 true false 120 195 180 255
Line -7500403 true 150 195 150 255
Rectangle -16777216 true false 30 180 105 240
Rectangle -16777216 true false 195 180 270 240
Line -16777216 false 0 165 300 165
Polygon -7500403 true true 0 165 45 135 60 90 240 90 255 135 300 165
Rectangle -7500403 true true 0 0 75 45
Rectangle -16777216 false false 0 0 75 45

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

computer workstation
false
0
Rectangle -7500403 true true 60 45 240 180
Polygon -7500403 true true 90 180 105 195 135 195 135 210 165 210 165 195 195 195 210 180
Rectangle -16777216 true false 75 60 225 165
Rectangle -7500403 true true 45 210 255 255
Rectangle -10899396 true false 249 223 237 217
Line -16777216 false 60 225 120 225

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fire
false
0
Polygon -7500403 true true 151 286 134 282 103 282 59 248 40 210 32 157 37 108 68 146 71 109 83 72 111 27 127 55 148 11 167 41 180 112 195 57 217 91 226 126 227 203 256 156 256 201 238 263 213 278 183 281
Polygon -955883 true false 126 284 91 251 85 212 91 168 103 132 118 153 125 181 135 141 151 96 185 161 195 203 193 253 164 286
Polygon -2674135 true false 155 284 172 268 172 243 162 224 148 201 130 233 131 260 135 282

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

hex
false
0
Polygon -7500403 true true 0 150 75 30 225 30 300 150 225 270 75 270

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
