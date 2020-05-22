namespace StardewValleyStonks
{
    public class Processor : Source
    {
        public bool PreservesQuality { get; set; }

        public Processor(
            string name,
            ICondition[] conditions = null)
            : base(name, conditions) { }
    }
}
