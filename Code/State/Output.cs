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

        private readonly Dictionary<Fertilizer, List<Fertilizer>> EqualAlternatesTo;
        Fertilizer[] Fertilizers;

        public Output()
        {
            EqualAlternatesTo = new Dictionary<Fertilizer, List<Fertilizer>>();
        }

        public void Calculate(Data data, Skills skills)
        {
            EqualAlternatesTo.Clear();
            Fertilizers = new LinkedList<Fertilizer>(
                data.Fertilizers.
                Select(f => f.ToFertilizer(skills.Farming.BuffedLevel))).
                DoComparisons(EqualAlternatesTo).
                ToArray();
        }
    }
}
