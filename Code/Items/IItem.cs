namespace StardewValleyStonks
{
    public interface IItem
    {
        string Name { get; }
        int Price { get; }
        QualityItem Normal { get; }

        QualityItem With(int quality);
    }
}