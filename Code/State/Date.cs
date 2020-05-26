using ExtentionsLibrary.Limits;

namespace StardewValleyStonks
{
    public class Date
    {
        public Seasons Seasons { get; private set; }
        public int Year
        {
            get => _Year;
            set => _Year = value.WithMin(1);
        }
        public Seasons StartSeason
        {
            get => _StartSeason;
            set
            {
                _StartSeason = value;
                UpdateSeasons();
            }
        }
        public Seasons EndSeason
        {
            get => _EndSeason;
            set
            {
                _EndSeason = value;
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

        private Seasons _StartSeason, _EndSeason;
        private int _Year, _StartDay, _EndDay;

        private void UpdateSeasons()
        {
            Seasons = 0;
            for (int season = (int)StartSeason; season < (int)EndSeason << 1; season <<= 1)
            {
                Seasons += season;
            }
        }

        public Date()
        {
            _Year = 1;
            StartSeason = Seasons.Spring;
            EndSeason = Seasons.Fall;
            _StartDay = 1;
            _EndDay = 28;
            UpdateSeasons();
        }
    }
}
