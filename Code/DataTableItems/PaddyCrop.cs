namespace StardewValleyStonks
{
    public class PaddyCrop : Crop
    {
        public bool Irrigated { get; set; }

        public override int GrowthTime(double speedMultiplier)
        {
            if (Irrigated)
            {
                speedMultiplier += 0.25; //make editable
            }
            return base.GrowthTime(speedMultiplier);
        }
    }
}
