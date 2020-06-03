namespace StardewValleyStonks
{
    public class PlanNode //: IEnumerable<PlanNode>?
    {
        public PlanSection Section { get; }
        public PlanNode Next { get; }

        public PlanNode(PlanSection section, PlanNode next)
        {
            Section = section;
            Next = next;
        }
        //public double TotalProfit => (Next == null ? 0 : Next.TotalProfit) + Profit;

        //public double Profit => 0;// Crop.Profit(Fertilizer.Quality, NumHarvests);

        public Item[] Products
        {
            get
            {
                //list of replant processes
                //list of sell processes

                //input --> process --> if only input display sell/seed
                //process --> sell/seed

                //check: num == 0 (don't display), seed == 0 (don't display seed), seed == num (don't display product), product == rawProduct (don't dispaly source)
                //num: raw product --source--> num: product (xg) for $total
                //          
                //num: raw product (xg) for $total
                //
                //                    |--- num --source--> num: product (xg) for $total
                //num: raw product ---|
                //                    |--- num --source--> num: seeds
                //
                //                    |--> num: raw product (xg) for $total
                //num: raw product ---|
                //                    |--- num --source--> num: seeds
                //
                //num: raw product --source--> num: seeds
                //
                //buy num: seed (xg) for $total
                return null;
            }
        }

        //add fertilzier cost   if((prev == null && startingFert == null) || prev.fert != fert) 
    }
}
