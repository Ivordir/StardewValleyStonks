using System.Collections.Generic;
using ExtentionsLibrary.Limits;

namespace StardewValleyStonks
{
    public class Date
    {
        public Seasons Seasons
        {
            get
            {
                Seasons seasons = 0;
                for (int season = (int)StartSeason; season < (int)EndSeason << 1; season <<= 1)
                {
                    seasons += season;
                }
                return seasons;
            }
        }
        public int Year
        {
            get => _Year;
            set => _Year = value.WithMin(1);
        }
        public Seasons StartSeason { get; set; }
        public Seasons EndSeason { get; set; }
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
        public bool Valid
        => StartSeason < EndSeason
            || StartSeason == EndSeason && StartDay < EndDay;

        int _Year, _StartDay, _EndDay;

        public int DaysInSeason(Seasons season)
        {
            if (Seasons.HasFlag(season))
            {
                if (StartSeason == EndSeason)
                {
                    return EndDay - StartDay;
                }
                else if (season == StartSeason)
                {
                    return 29 - StartDay;
                }
                else if (season == EndSeason)
                {
                    return EndDay;
                }
                return 28;
            }
            return 0;
        }

        public IEnumerable<Seasons> SingleSeasons()
        {
            for (int season = (int)StartSeason; season < (int)EndSeason << 1; season <<= 1)
            {
                yield return (Seasons)season;
            }
        }

        IEnumerable<int> SeasonsToInts()
        {
            for (int season = (int)StartSeason; season < (int)EndSeason << 1; season <<= 1)
            {
                yield return season;
            }
        }

        public Date()
        {
            _Year = 1;
            StartSeason = Seasons.Spring;
            EndSeason = Seasons.Fall;
            _StartDay = 1;
            _EndDay = 28;
        }
    }
}
