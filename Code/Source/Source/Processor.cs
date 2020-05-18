namespace StardewValleyStonks
{
    public class Processor : Source
    {
        public bool PreservesQuality { get; set; }

        public Processor(
            string name,
            bool preservesQuality = false,
            bool enabled = true,
            ICondition[] conditions = null)
            : base(name, enabled, conditions)
        {
            PreservesQuality = preservesQuality;
        }
    }
}
