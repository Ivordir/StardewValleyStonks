using System.Collections.Generic;

public class Fertilizer : SourcedItem
{
    public static Fertilizer None = new Fertilizer("None", 0, 0);

    public int Quality { get; }
    public float Speed { get; }

    public Fertilizer(string name, int quality, float speed) : base(name)
    {
        Quality = quality;
        Speed = speed;
    }

    public Fertilizer(string name, int quality, float speed, Dictionary<Source, int> priceFrom) : base(name, priceFrom)
    {
        Quality = quality;
        Speed = speed;
    }
}
