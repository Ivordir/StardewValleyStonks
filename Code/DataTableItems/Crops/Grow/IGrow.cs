namespace StardewValleyStonks
{
    public interface IGrow
    {
        int TotalTime { get; }

        int Time(double speedMultiplier);
        int HarvestsWithin(int days, double speed = 0);
        int HarvestsWithin(ref int days, double speed = 0);
    }
}