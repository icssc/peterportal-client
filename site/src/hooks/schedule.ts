import { useEffect } from 'react';
import { useAppDispatch, useAppSelector } from '../store/hooks';
// import { fetchCurrentWeek, fetchCurrentQuarter } from '../store/slices/scheduleSlice';
import { setCurrentWeek, fetchCurrentQuarter } from '../store/slices/scheduleSlice';
import trpc from '../trpc';

export function useCurrentWeek() {
  const dispatch = useAppDispatch();
  const currentWeek = useAppSelector((state) => state.schedule.currentWeek);

  useEffect(() => {
    // if (!currentWeek) {
    //   dispatch(fetchCurrentWeek());
    // }
    if (currentWeek) return;
    trpc.schedule.currentWeek.query().then((res) => {
      const week = res.display.split(' • ')[0];
      dispatch(setCurrentWeek(week));
    });
  }, [currentWeek, dispatch]);

  return { currentWeek };
}

export function useCurrentQuarter() {
  const dispatch = useAppDispatch();
  const currentQuarter = useAppSelector((state) => state.schedule.currentQuarter);

  useEffect(() => {
    if (!currentQuarter) {
      dispatch(fetchCurrentQuarter());
    }
  }, [currentQuarter, dispatch]);

  return { currentQuarter };
}
