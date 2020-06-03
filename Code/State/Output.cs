using System.Collections.Generic;
using System.Linq;

namespace StardewValleyStonks
{
    public class Output
    {
        Crop[] Crops;
        Fertilizer[] Fertilizers;
        readonly List<Plan> Plans;
        readonly Dictionary<Fertilizer, List<Fertilizer>> EqualAlternatesTo;
        readonly Data Data;
        readonly Date Date;
        readonly Skills Skills;
        readonly Settings Settings;

        public Output(Data data, Date date, Skills skills, Settings settings)
        {
            Data = data;
            Date = date;
            Skills = skills;
            Settings = settings;
            EqualAlternatesTo = new Dictionary<Fertilizer, List<Fertilizer>>();

            CropIn = new Dictionary<Seasons, Crop[]>();
        }

        public void Calculate()
        {
            EqualAlternatesTo.Clear();
            Fertilizers = new LinkedList<Fertilizer>(
                Data.Fertilizers.
                Select(f => f.ToFertilizer(Skills.Farming.BuffedLevel))).
                DoComparisons(EqualAlternatesTo).
                ToArray();
            Crops = Data.Crops.Where(c => c.Active).Select(c => c.ToCrop()).ToArray();

            CropIn.Clear();
            foreach(Seasons season in Date.SingleSeasons())
            {
                CropIn.Add(season, Crops.Where(c => c.GrowsIn(season)).ToArray());
            }
        }

        readonly Dictionary<Seasons, Crop[]> CropIn;
        List<PlanNode> FirstRecursiveDraft(IEnumerable<Seasons> seasons, Dictionary<Seasons, int> daysIn)
        {
            //ignore for now : multi-season crops
            //FertilizerDestroyed, FertilizerCompatable, IndoorsOnly
            List<PlanNode> plans = new List<PlanNode>();
            foreach(Seasons season in seasons)
            {
                foreach(Fertilizer fert in Fertilizers)
                {
                    plans.AddRange(FirstDraftHelper(season, fert, daysIn[season]));
                    //uhh.. somehow permute, or maybe not yet? plans from each season.
                }
            }
            return plans;
        }

        List<PlanNode> FirstDraftHelper(Seasons season, Fertilizer fert, int days, bool oneRegrow = false)
        {
            List<PlanNode> plans = new List<PlanNode>(); 
            foreach (Crop crop in CropIn[season])
            {
                //Growth time cannot be equal days; must be less than days.
                //E.g. a crop that grows in 28 days cannot give one harvest within a season (ancient fruit).
                if (crop.GrowthTimeWith(fert) < days && (!crop.Regrows || !oneRegrow))
                {
                    int daysNotUsed = days;
                    PlanSection section = new PlanSection(crop, fert, crop.HarvestsWithin(ref daysNotUsed, fert));
                    plans.AddRange(FirstDraftHelper(season, fert, daysNotUsed, crop.Regrows || oneRegrow).
                        Select(n => new PlanNode(section, n)));
                }
            }
            if (plans.Count == 0)
            {
                plans.Add(null);
            }
            return plans;
        }
    }
}
