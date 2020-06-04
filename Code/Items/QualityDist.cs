namespace StardewValleyStonks
{
    public readonly struct QualityDist
    {
        public readonly double Value { get; }
        public readonly int Quality { get; }
        public readonly double AllQualities
        {
            get
            {
                double sum = Value;
                for (int quality = Quality + 1; quality < Dist.Length; quality++)
                {
                    sum += Dist[quality];
                }
                return sum;
            }
        }
        public readonly int Length => Dist.Length;
        //public readonly int QualitiesLeft => Dist.Length - Quality;
        public readonly bool Empty => Quality == Dist.Length;
        public readonly QualityDist Next => new QualityDist(Quality + 1, Dist);

        public static QualityDist operator -(QualityDist dist, double amount)
        => amount == dist.Value
            ? dist.Next
            : new QualityDist(dist - amount, dist, dist.Dist);

        //public static implicit operator double[](QualityDist dist) => dist.Dist[..];
        public static implicit operator QualityDist(double[] dist) => new QualityDist(dist);
        public static implicit operator double(QualityDist dist) => dist.Value;
        public static implicit operator int(QualityDist dist) => dist.Quality;

        readonly double[] Dist;

        QualityDist(double[] dist)
        {
            Value = dist[0];
            Quality = 0;
            Dist = dist;
        }
        QualityDist(double value, int quality, double[] dist)
        {
            Value = value;
            Quality = quality;
            Dist = dist;
        }
        QualityDist(int quality, double[] dist)
        {
            Value = quality == dist.Length ? 0 : dist[quality];
            Quality = quality;
            Dist = dist;
        }
    }
}
