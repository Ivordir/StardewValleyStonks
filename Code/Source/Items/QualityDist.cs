using System.Linq;

namespace StardewValleyStonks
{
    public readonly struct QualityDist
    {
        public double AllQualities => Dist.Sum();

        public static QualityDist operator -(QualityDist dist, double amount)
            => amount == dist.Value ?
                new QualityDist(dist.Quality + 1, dist.Dist)
                : new QualityDist(dist.Value - amount, dist.Quality, dist.Dist);

        public static implicit operator QualityDist(double[] dist) => new QualityDist(dist);
        public static implicit operator double(QualityDist dist) => dist.Value;

        private readonly double Value;
        private readonly int Quality;
        private readonly double[] Dist;

        public QualityDist(double[] dist)
        {
            Value = dist[0];
            Quality = 0;
            Dist = dist;
        }
        private QualityDist(double value, int quality, double[] dist)
        {
            Value = value;
            Quality = quality;
            Dist = dist;
        }
        private QualityDist(int quality, double[] dist)
        {
            Value = quality == dist.Length ? 0 : dist[quality];
            Quality = quality;
            Dist = dist;
        }
    }
}
