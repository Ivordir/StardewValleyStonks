namespace StardewValleyStonks

module internal AppCache =
  let private cacheLast keyFun inputFun defaultInput defaultValue initCalc =
    let mutable calc = initCalc
    let cache = Cache()

    (fun a ->
      (match cache.TryFind (keyFun a) with
      | Some getOrRecalc -> getOrRecalc
      | None ->
          let mutable prevOutput = defaultValue
          let mutable prevInput = defaultInput
          
          let getOrRecalc a2 =
            let input = inputFun a2
            if input != prevInput then
              if input <> prevInput then
                prevOutput <- calc a2
              prevInput <- input
            prevOutput
          
          cache.Add(keyFun a, getOrRecalc)
          getOrRecalc)
        a),
    (fun newCalc ->
      cache.Clear()
      calc <- newCalc)

  let (lowestPrice: _ -> int option), newLowestCalc = cacheLast Prices.id Prices.selected Set.empty None (fun _ -> None)
  let (unitProfit: _ -> float array option), newProfitCalc = cacheLast RawCrop.item RawCrop.selected Set.empty None (fun _ -> None)



type Message =
  | SetPage of Page
  | SetSettingsTab of SettingsTab
  | SetCropTableTab of CropTableTab
  | SetCropTab of CropTab
  | SetFertilizerTableTab of FertilizerTableTab

  | SetSkillLevel of Skills * int
  | SetSkillBuff of Skills * int
  | ToggleProfession of Skills * Profession
  | ToggleIgnoreProfessionRelationships

  | ToggleRawMultiplier of RawMultiplier
  | TogglePriceMultiplier of PriceMultiplier
  
  | ToggleSeedPrice of Crop name * Price
  | ToggleSeedSource of Source
  | ToggleBuySeeds of Crop name
  | ToggleAllBuySeeds
  | ToggleProduct of Crop name * Item * Product
  | ToggleProductProcessor of Processor name option
  | ToggleReplant of Crop name * Item
  | ToggleReplantProcessor of Processor name option

  | ToggleCrop of Crop name
  | ToggleAllCrops
  | SetCropSort of CropSort
  // | ToggleInSeasonFilter
  // | ToggleProfitableFilter
  // | ToggleCrossSeasonFilter
  // | SetCropTypeFilter of CropType

  // | ToggleAllowCropClearings
  // | ToggleAllowCrossSeason
  | ToggleAccountForReplant

  | ToggleFertilizerPrice of Fertilizer name * Price
  | ToggleFertilizerSource of Source
  | ToggleFertilizer of Fertilizer name
  | ToggleAllFertilizers
  | SetFertilizerSort of FertilizerSort
  | ToggleAccountForFertilizerCost
  | ToggleAccountForReplacementFertilizer

  | SetStartDay of int
  | SetStartSeason of Season
  | SetEndDay of int
  | SetEndSeason of Season

  // | ToggleShowUnprofitable
  | SetSelectedPair of (Crop name * Fertilizer name option) option
  | SetCompareMode of CompareMode

  | SetStartingFertilizer of Fertilizer name option

  | SetSkillLevelPolicy of RequirementPolicy
  | ToggleSpecialCharm
  | SetLuckBuff of int

  | SetGiantCropChecksPerTile of float

  | ToggleQualityProducts
  | ToggleQualityProcessor of Processor name
  | ToggleQualitySeedMaker
  | SetQualitySeedMakerAmount of Quality * Amount: int


module Update =
  open Elmish.Navigation

  let urlUpdate page (ui, data) =
    match page with
    | Some x -> ( { ui with Page = x }, data), []
    | None -> (ui, data), Navigation.modifyUrl (Page.url ui.Page)

  let update message (ui, data) =
    match message with
    | SetPage page -> ( { ui with Page = page }, data), Navigation.newUrl (Page.url page)
    | SetSettingsTab tab -> ( { ui with SettingsTab = tab }, data), []
    | SetCropTableTab tab -> ( { ui with CropTableTab = tab }, data), []
    | SetCropTab tab -> ( { ui with CropTab = tab }, data), []
    | SetFertilizerTableTab tab -> ( { ui with FertilizerTableTab = tab }, data), []

    | SetSkillLevel (skill, level) ->
        let oldLevel = data.Skills.[skill].Level
        let newLevel = Skill.validLevel level
        if newLevel = oldLevel then
          (ui, data), []
        else
          let newModel = { data with Skills = data.Skills |> Map.update skill (Skill.setLevel level) }

          let processorChanged =
            newModel.SkillLevelPolicy = Enforce
            && newModel.Processors
              |> Map.values
              |> Seq.exists (fun pro -> Processor.unlocked newModel.Skills.Find pro <> Processor.unlocked data.Skills.Find pro)

          let changedProfessions =
            if newModel.SkillLevelPolicy = Enforce then
              newModel.ProfessionMultipliers
              |> Seq.filter (fun (s, p, _) ->
                DataModel.professionActiveAlt newModel s p <> DataModel.professionActiveAlt data s p)
              |> Seq.map Profession
            else
              Seq.empty

          if skill = Farming && (processorChanged || changedProfessions |> Seq.exists newModel.ProductMultipliers.Contains) then
            AppCache.newProfitCalc <| DataModel.rawCropUnitProfit newModel
          
          (ui, newModel), []

    | SetSkillBuff (skill, buff) -> (ui, { data with Skills = data.Skills |> Map.update skill (Skill.setBuff buff) } ), []
    
    | ToggleProfession (skill, profession) ->
        let newModel = { data with Skills = data.Skills |> Map.update skill ((if ui.IgnoreProfessionRelationships then Skill.toggleSingleProfession else Skill.toggleProfession) profession) }
        
        if skill = Farming && (newModel.SkillLevelPolicy <> Enforce || Skill.professionUnlocked data.Skills.[skill] profession) then
          let changedProfessions =
            if newModel.SkillLevelPolicy = Enforce then
              newModel.ProfessionMultipliers
              |> Seq.filter (fun (s, p, _) ->
                DataModel.professionActiveAlt newModel s p <> DataModel.professionActiveAlt data s p)
              |> Seq.map Profession
            else
              Seq.empty

          if changedProfessions |> Seq.exists newModel.ProductMultipliers.Contains then
            AppCache.newProfitCalc <| DataModel.rawCropUnitProfit newModel

        (ui, newModel), []

    | ToggleIgnoreProfessionRelationships -> ( { ui with IgnoreProfessionRelationships = not ui.IgnoreProfessionRelationships }, data), []

    | ToggleRawMultiplier multiplier ->
        let m = Raw multiplier
        let newModel = { data with SelectedRawMultipliers = data.SelectedRawMultipliers |> Set.addOrRemove multiplier }

        if newModel.ProductMultipliers.Contains m then
          AppCache.newProfitCalc <| DataModel.rawCropUnitProfit newModel

        (ui, newModel), []

    | TogglePriceMultiplier multiplier ->
        let newModel = { data with SelectedPriceMultipliers = data.SelectedPriceMultipliers |> Set.addOrRemove multiplier }
        AppCache.newLowestCalc <| DataModel.lowestPrice newModel
        (ui, newModel), []

    | ToggleSeedPrice (name, price) ->
        (ui, { data with Crops = data.Crops |> Map.update name (Crop.togglePrice price) } ), []

    | ToggleSeedSource source ->
        let select =
          data.Crops
          |> Map.values
          |> Seq.exists (Crop.seedPrices >> Option.forall (Prices.sourceSelected source) >> not)
        
        (ui, { data with Crops = data.Crops |> Map.mapByValue (Crop.setPriceSelected select source) } ), []

    | ToggleBuySeeds name ->
        (ui, { data with Crops = data.Crops |> Map.update name Crop.toggleBuySeeds } ), []

    | ToggleAllBuySeeds ->
        let select = data.Crops |> Map.values |> Seq.exists (Crop.seedSelected >> Option.forall id >> not)
        (ui, { data with Crops = data.Crops |> Map.mapByValue (Crop.setBuySeeds select) } ), []

    | ToggleProduct (name, item, product) ->
        (ui, { data with Crops = data.Crops |> Map.update name (Crop.toggleProduct product item) } ), []

    | ToggleProductProcessor processor ->
        let select = data.Crops |> Map.values |> Seq.collect Crop.rawCrops |> Seq.exists (RawCrop.processorSelected processor >> not)
        (ui, { data with Crops = data.Crops |> Map.mapByValue (Crop.setSelectedProduct select processor) } ), []

    | ToggleReplant (name, item) ->
        (ui, { data with Crops = data.Crops |> Map.update name (Crop.toggleReplant item) } ), []

    | ToggleReplantProcessor processor ->
        let select = data.Crops |> Map.values |> Seq.collect Crop.rawCrops |> Seq.exists (not << RawCrop.replantProcessorSelected processor)

        (ui, { data with Crops = data.Crops |> Map.mapByValue (Crop.setSelectedReplant select processor) } ), []

    | ToggleCrop crop -> (ui, { data with CropSelection = data.CropSelection |> Set.addOrRemove crop } ), []

    | ToggleAllCrops ->
        ( ui,
          { data with
              CropSelection =
                if data.CropSelection.Count = data.Crops.Count
                then Set.empty
                else data.Crops |> Map.keys |> set } ), []
              
    | SetCropSort sort ->
        ((if sort = ui.CropSort then
            { ui with CropSortAscending = not ui.CropSortAscending }
          else
            { ui with
                CropSort = sort
                CropSortAscending = true } ),
          data),
        []

    // | ToggleInSeasonFilter -> { model with InSeasonFilter = not model.InSeasonFilter }, []
    // | ToggleProfitableFilter -> { model with ProfitableFilter = not model.ProfitableFilter }, []
    // | ToggleCrossSeasonFilter -> { model with CrossSeasonFilter = not model.CrossSeasonFilter }, []
    // | SetCropTypeFilter filter -> { model with CropTypeFilter = if Some filter = model.CropTypeFilter then None else Some filter }, []

    | ToggleAccountForReplant ->
        (ui, { data with AccountForReplant = not data.AccountForReplant } ), []

    | ToggleAccountForReplacementFertilizer ->
        (ui, { data with AccountForReplacementFertilizer = not data.AccountForReplacementFertilizer } ), []

    | ToggleFertilizerPrice (name, price) ->
        (ui, { data with Fertilizers = data.Fertilizers |> Map.update name (Fertilizer.togglePrice price) } ), []

    | ToggleFertilizerSource source ->
        let select =
          data.Fertilizers
          |> Map.values
          |> Seq.exists (Fertilizer.prices >> Prices.sourceSelected source >> not)
        
        (ui, { data with Fertilizers = data.Fertilizers |> Map.mapByValue (Fertilizer.setPriceSelected select source) } ), []

    | ToggleFertilizer fert ->
        (ui, { data with FertilizerSelection = data.FertilizerSelection |> Set.addOrRemove fert } ), []
    
    | ToggleAllFertilizers ->
        ( ui,
          { data with
              FertilizerSelection =
                if data.FertilizerSelection.Count = data.Fertilizers.Count
                then Set.empty
                else data.Fertilizers |> Map.keys |> set } ), []

    | SetFertilizerSort sort ->
        ((if sort = ui.FertilizerSort then
            { ui with FertilizerSortAscending = not ui.FertilizerSortAscending }
          else
            { ui with
                FertilizerSort = sort
                FertilizerSortAscending = true } ),
          data), []

    | ToggleAccountForFertilizerCost -> (ui, { data with AccountForFertilizerCost = not data.AccountForFertilizerCost } ), []

    | SetStartDay day -> (ui, { data with StartDate = data.StartDate |> Date.withDay day } ), []

    | SetStartSeason season -> (ui, { data with StartDate = data.StartDate |> Date.withSeason season } ), []

    | SetEndDay day -> (ui, { data with EndDate = data.EndDate |> Date.withDay day } ), []
        
    | SetEndSeason season -> (ui, { data with EndDate = data.EndDate |> Date.withSeason season } ), []

    //| ToggleShowUnprofitable -> ( { ui with ShowUnprofitable = not ui.ShowUnprofitable }, data), []

    | SetSelectedPair pair ->
        ( { ui with SelectedPair = if ui.SelectedPair = pair then None else pair }, data), []

    | SetCompareMode mode -> (ui, { data with CompareMode = mode } ), []

    | SetStartingFertilizer fert -> (ui, { data with StartingFertilizer = fert } ), []

    | SetSkillLevelPolicy policy ->
        let newModel = { data with SkillLevelPolicy = policy }

        if (data.SkillLevelPolicy = Enforce) <> (policy = Enforce) then
          let processorChanged =
            newModel.Processors
            |> Map.values
            |> Seq.exists (not << Processor.unlocked newModel.Skills.Find)
          
          let changedProfessions =
            newModel.ProfessionMultipliers
            |> Seq.filter (fun (s, p, _) ->
              let skill = newModel.Skills.[s]
              Skill.professionSelected skill p && not <| Skill.professionUnlocked skill p)
            |> Seq.map Profession

          if processorChanged || changedProfessions |> Seq.exists newModel.ProductMultipliers.Contains then
            AppCache.newProfitCalc <| DataModel.rawCropUnitProfit newModel

        (ui, newModel), []

    | ToggleSpecialCharm ->
        let charm = not data.SpecialCharm
        ( ui,
          { data with
              SpecialCharm = charm
              DoubleCropProb = CropAmount.doubleCropProb data.LuckBuff charm } ), []

    | SetLuckBuff buff ->
        let b = CropAmount.validBuff buff
        (ui,
          { data with
              LuckBuff = b
              DoubleCropProb = CropAmount.doubleCropProb buff data.SpecialCharm } ), []
    
    | SetGiantCropChecksPerTile checks ->
        let c = CropAmount.validGiantChecks checks
        (ui,
          { data with
              GiantChecksPerTile = c
              NoGiantProb = CropAmount.noGiantCropProb data.BaseGiantProb c } ), []

    | ToggleQualityProducts ->
        let newModel = { data with QualityProducts = not data.QualityProducts }
        AppCache.newProfitCalc <| DataModel.rawCropUnitProfit newModel
        (ui, newModel), []

    | ToggleQualityProcessor name ->
        let newModel = { data with Processors = data.Processors |> Map.update name Processor.toggleQualityProcessor }
        AppCache.newProfitCalc <| DataModel.rawCropUnitProfit newModel
        (ui, newModel), []

    | ToggleQualitySeedMaker ->
        let seedMaker = Name "Seed Maker"
        let newModel =
          if data.QualitySeedMaker then
            { data with
                QualitySeedMaker = false
                Processors = data.Processors |> Map.update seedMaker (Processor.withAmount (Some (QualityIndependent <| data.SeedMakerSeedProb * float data.SeedMakerSeedAmount))) }
          else
            { data with
                QualitySeedMaker = true
                Processors = data.Processors |> Map.update seedMaker (Processor.withAmount (Some (QualityDependent (data.QualitySeedMakerAmounts |> Array.map float)))) }

        if DataModel.processorActive newModel seedMaker then
          AppCache.newProfitCalc <| DataModel.rawCropUnitProfit newModel

        (ui, newModel), []
    
    | SetQualitySeedMakerAmount (quality, amount) ->
        let a = positive amount
        if data.QualitySeedMakerAmounts.[int quality] = a then
          (ui, data), []
        else
          let seedMaker = Name "Seed Maker"
          let amounts = Array.copy data.QualitySeedMakerAmounts
          amounts.[int quality] <- a
          let newModel =
            { data with
                QualitySeedMakerAmounts = amounts
                Processors = data.Processors |> Map.update seedMaker (Processor.withAmount (Some (QualityDependent (amounts |> Array.map float)))) }
          
          if newModel.QualitySeedMaker && DataModel.processorActive newModel seedMaker then
            AppCache.newProfitCalc <| DataModel.rawCropUnitProfit newModel
         
          (ui, newModel), []
