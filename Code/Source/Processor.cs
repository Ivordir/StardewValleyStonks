namespace StardewValleyStonks
{
    public class Processor : Source
    {
        public bool PreservesQualityDIO { get; set; }
        public bool PreservesQuality { get; private set; }

        public void Save()
        {
            PreservesQuality = PreservesQualityDIO;
        }

        public Processor(
            string name,
            ICondition[] conditions = null,
            bool preservesQuality = false)
            : base(name, conditions)
        {
            PreservesQualityDIO = preservesQuality;
            PreservesQuality = preservesQuality;
        }
    }
}
