namespace StardewValleyStonks
{
    public class PlanNode
    {
        public int NumHarvests { get; }
        public CropDIO Crop { get; }
        public FertilizerDIO Fertilizer { get; }
        private readonly PlanNode PrevNode;

        public PlanNode(CropDIO crop, FertilizerDIO fertilizer, PlanNode node = null)
        {
            Crop = crop;
            Fertilizer = fertilizer;
            PrevNode = node;
        }
        public double TotalProfit => (PrevNode == null ? 0 : PrevNode.TotalProfit) + Profit;

        public double Profit => Crop.Profit(Fertilizer.Quality, NumHarvests);

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
