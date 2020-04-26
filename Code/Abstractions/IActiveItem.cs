namespace StardewValleyStonks
{
    public interface IActiveItem
    {
        public bool Enabled { get; set; }
        public ICondition[] Conditions { get; }

        public bool ConditionsMet { get; }
        public bool Active { get; }
    }
}