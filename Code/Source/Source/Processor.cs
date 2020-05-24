namespace StardewValleyStonks
{
    public class Processor : Source
    {
        public bool PreservesQuality { get; set; }

        public Processor(
            string name,
            ICondition[] conditions = null,
            bool preservesQuality = false)
            : base(name, conditions)
        {
            PreservesQuality = preservesQuality;
        }
    }
}
