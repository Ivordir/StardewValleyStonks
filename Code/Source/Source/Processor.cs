namespace StardewValleyStonks
{
    public class Processor : Source
    {
        public bool PreservesQuality { get; set; }

        public Processor(
            string name,
            bool preservesQuality = false,
            ICondition[] conditions = null)
            : base(name, conditions)
        {
            PreservesQuality = preservesQuality;
        }
    }
}
