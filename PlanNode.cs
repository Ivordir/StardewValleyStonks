public class PlanNode
{
    public int EndSeason { get; }
    public int DaysGrown { get; }
    public int NumHarvests { get; }
    public Crop Crop { get; }
    public Fertilizer Fertilizer { get; }
    public bool MultiSeason { get; }
    private readonly PlanNode PrevNode;

    public PlanNode(int endSeason, int daysGrown, Crop crop, Fertilizer fertilizer, PlanNode node = null, bool multiSeason = false)
    {
        EndSeason = endSeason;
        DaysGrown = daysGrown;
        Crop = crop;
        Fertilizer = fertilizer;
        PrevNode = node;
        MultiSeason = multiSeason;
    }
    public double TotalProfit()
    {
        return (PrevNode == null ? Profit() : PrevNode.TotalProfit()) + Profit();
    }

    public double Profit()
    {
        return Crop.Profit(NumHarvests, Fertilizer.Quality);
    }
}
