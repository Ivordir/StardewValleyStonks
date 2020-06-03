namespace StardewValleyStonks
{
    public class PlanSection
    {
        public Crop Crop { get; }
        public Fertilizer Fertilizer { get; }
        public int Harvests { get; }

        public PlanSection(Crop crop, Fertilizer fertilizer, int harvests)
        {
            Crop = crop;
            Fertilizer = fertilizer;
            Harvests = harvests;
        }
    }
}
