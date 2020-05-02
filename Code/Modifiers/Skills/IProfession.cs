namespace StardewValleyStonks
{
    public interface IProfession : ISelectable
    {
        public IProfession[] Dependants { get; }
        public IProfession[] Requirements { get; }
        public IProfession[] ExclusiveWith { get; }
    }
}