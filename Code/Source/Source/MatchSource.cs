namespace StardewValleyStonks
{
    public class MatchSource : Source
    {
        public bool Match { get; set; }

        public MatchSource(
            string name,
            ICondition[] conditions = null,
            bool match = false)
            : base(name, conditions)
        {
            Match = match;
        }
    }
}
