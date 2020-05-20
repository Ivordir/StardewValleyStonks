namespace StardewValleyStonks
{
    public class Source : Selectable
    {
        public string Name { get; }

        public Source(
            string name,
            bool enabled = true,
            ICondition[] conditions = null)
            : base(enabled, conditions)
        {
            Name = name;
        }
    }
}
