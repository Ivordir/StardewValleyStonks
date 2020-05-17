namespace StardewValleyStonks
{
    public class SelectableMultiplier : Selectable, IMultiplier
    {
        public string Name { get; }
        public double Value => Active ? ActiveValue : InActiveValue;

        private readonly double ActiveValue;
        private readonly double InActiveValue;

        public SelectableMultiplier(
            string name,
            double activeValue,
            double inactiveValue,
            bool selected = false,
            ICondition[] conditions = null)
            : base(selected, conditions)
        {
            Name = name;
            ActiveValue = activeValue;
            InActiveValue = inactiveValue;
        }
    }
}
