namespace StardewValleyStonks
{
    public class DateState
    {
        public int Year
        {
            get => Year;
            set => value.WithMin(1);
        }
        public Season Seasons { get; private set; }
        public int StartSeason
        {
            get => StartSeason;
            set
            {
                StartSeason = value;
                UpdateSeasons();
            }
        }
        public int EndSeason
        {
            get => EndSeason;
            set
            {
                StartSeason = value;
                UpdateSeasons();
            }
        }
        public int StartDay
        {
            get => StartDay;
            set => value.InRange(1, 28);
        }
        public int EndDay
        {
            get => EndDay;
            set => value.InRange(1, 28);
        }

        private void UpdateSeasons()
        {
            Seasons = 0;
            for (int season = 1 << StartSeason - 1; season < 1 << EndSeason; season <<= 1)
            {
                Seasons += season;
            }
        }

        public DateState()
        {
            Year = 1;
            StartSeason = 1;
            EndSeason = 3;
            StartDay = 1;
            EndDay = 28;
            UpdateSeasons();
        }
    }
}
