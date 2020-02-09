public class Fertilizer : ItemWithSources
{
    public string Name { get; }
    public int Quality { get; }
    public float Speed { get; }

    public Fertilizer(string name, int quality, float speed, Dictionary<string, int> priceFrom) : base(priceFrom)
    {
        Name = name;
        Quality = quality;
        Speed = speed;
    }
}
