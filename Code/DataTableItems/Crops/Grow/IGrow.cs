namespace StardewValleyStonks
{
    public interface IGrow
    {
        public int TotalTime { get; }
        public bool Regrows { get; }
        public int RegrowTime { get; }
        public int Time(double speedMultiplier);
        public int HarvestsWithin(int days, double speed = 0);
        public int HarvestsWithin(ref int days, double speed = 0);
    }
}