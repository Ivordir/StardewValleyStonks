namespace StardewValleyStonks
{
    public class Source : Selectable
    {
        public string Name { get; }

        public Source(
            string name,
            ICondition[] conditions = null)
            : base(true, conditions)
        {
            Name = name;
        }
    }
}
