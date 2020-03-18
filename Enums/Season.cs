[System.Flags]
public enum Season
{
  Spring = 1,
  Summer = 1 << 1,
  Fall = 1 << 2,
  Winter = 1 << 3
}

public static class Seasons
{
    private const int NumSeasons = 4;

    public static int Count => NumSeasons;
}