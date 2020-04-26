namespace StardewValleyStonks
{
    public class StaticCrop
    {
        public Season Seasons { get; }
        public IProductSource ProductSource { get; }
        public IProduct Product {get;}
        protected readonly IRanker<ISource, IReplant> Replant;

        private readonly int GrowthTime;
        private readonly int[] GrowthStagesOrg;
        private readonly int[] GrowthStages;
        private readonly double AvgExtraCrops;
    }
}
