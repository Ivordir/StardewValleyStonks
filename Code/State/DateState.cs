namespace StardewValleyStonks
{
    public class DateState
    {
        public int Year
        {
            get => _Year;
            set => _Year = value.WithMin(1);
        }
        public int StartSeason
        {
            get => _StartSeason;
            set
            {
                _StartSeason = value.InRange(1, 4);
                UpdateSeasons();
            }
        }
        public int EndSeason
        {
            get => _EndSeason;
            set
            {
                _EndSeason = value.InRange(1, 4);
                UpdateSeasons();
            }
        }
        public int StartDay
        {
            get => _StartDay;
            set => _StartDay = value.InRange(1, 28);
        }
        public int EndDay
        {
            get => _EndDay;
            set => _EndDay = value.InRange(1, 28);
        }
        public Season Seasons { get; private set; }

        private int _Year, _StartSeason, _EndSeason, _StartDay, _EndDay;

        public DateState()
        {
            _Year = 1;
            _StartSeason = 1;
            _EndSeason = 3;
            _StartDay = 1;
            _EndDay = 28;
            UpdateSeasons();
        }

        private void UpdateSeasons()
        {
            Seasons = 0;
            for (int season = 1 << StartSeason - 1; season < 1 << EndSeason; season <<= 1)
            {
                Seasons += season;
            }
        }
    }
}
