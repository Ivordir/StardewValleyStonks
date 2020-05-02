namespace StardewValleyStonks
{
    public class Source : Selectable, ISource
    {
        public string Name { get; }

        public Source(string name, bool enabled = true, ICondition[] conditions = null) : base(enabled, conditions)
        {
            Name = name;
        }

        public override int GetHashCode()
        {
            return System.HashCode.Combine(Name);
        }
    }
}
