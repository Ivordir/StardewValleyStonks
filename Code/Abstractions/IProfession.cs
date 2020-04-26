namespace StardewValleyStonks
{
    public interface IProfession
    {
        public bool Active { get; }
        public bool Enabled { get; set; }
        public ICondition LvlCondition { get; }
    }
}