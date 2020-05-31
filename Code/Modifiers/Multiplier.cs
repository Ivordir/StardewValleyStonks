namespace StardewValleyStonks
{
    public class Multiplier : Selectable, IMultiplier
    {
        public string Name { get; }
        public double Value { get; }
        public override bool Active => _Active;

        //temporary way to save whether a multiplier applies to items used in output.
        public void Save()
        {
            _Active = base.Active;
        }

        bool _Active;

        public Multiplier(
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
