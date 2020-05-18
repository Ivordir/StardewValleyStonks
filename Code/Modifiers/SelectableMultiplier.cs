namespace StardewValleyStonks
{
    public class SelectableMultiplier : Selectable, IMultiplier
    {
        public string Name { get; }
        public double Value { get; }

        public SelectableMultiplier(
            string name,
            double value,
            bool selected = true,
            ICondition[] conditions = null)
            : base(selected, conditions)
        {
            Name = name;
            Value = value;
        }
    }
}
