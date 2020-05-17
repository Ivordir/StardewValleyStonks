namespace StardewValleyStonks
{
    public class DataState
    {
        public Crop[] Crops { get; }
        public Fertilizer[] Fertilizers { get; }

        public IProcessor[] Processors { get; }
        public ISource[] BuySources { get; }
        public ISource[] ReplantMethods { get; }

        public DataState()
        {
            Crops = new Crop[0];
            Fertilizers = new Fertilizer[0];

            Processors = new IProcessor[0];
            BuySources = new ISource[0];
            ReplantMethods = new ISource[0];
        }
    }
}
