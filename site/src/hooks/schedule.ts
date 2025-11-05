import { useEffect } from 'react';
import { useAppDispatch, useAppSelector } from '../store/hooks';
import { setCurrentWeek, setCurrentQuarter } from '../store/slices/scheduleSlice';
import trpc from '../trpc';

export function useCurrentWeek() {
  const dispatch = useAppDispatch();
  const currentWeek = useAppSelector((state) => state.schedule.currentWeek);

  useEffect(() => {
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
    if (currentQuarter) return;
    trpc.schedule.currentQuarter.query().then((res) => {
      dispatch(setCurrentQuarter(res));
    });
  }, [currentQuarter, dispatch]);

  return { currentQuarter };
}
