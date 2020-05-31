using System.Collections.Generic;
using System.Linq;

namespace StardewValleyStonks
{
    public class Output
    {
        //StaticCrop[] Crops;
        //StaticRegrowCrop[] RegrowCrops;
        //StaticFertilizer[] Fertilizers;
        //List<PlanNode> Plans;

        readonly Dictionary<Fertilizer, List<Fertilizer>> EqualAlternatesTo;
        readonly Data Data;
        readonly Skills Skills;
        Fertilizer[] Fertilizers;

        public Output()
        {
            EqualAlternatesTo = new Dictionary<Fertilizer, List<Fertilizer>>();
        }

        public void Calculate()
        {
            EqualAlternatesTo.Clear();
            Fertilizers = new LinkedList<Fertilizer>(
                Data.Fertilizers.
                Select(f => f.ToFertilizer(Skills.Farming.BuffedLevel))).
                DoComparisons(EqualAlternatesTo).
                ToArray();
        }
    }
}
