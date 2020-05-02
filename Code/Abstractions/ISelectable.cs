namespace StardewValleyStonks
{
    public interface ISelectable
    {
        public bool Selected { get; set; }
        public ICondition[] Conditions { get; }

        public bool ConditionsMet { get; }
        public bool Active { get; }
    }
}