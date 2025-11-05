import { useEffect } from 'react';
import { useAppDispatch } from '../store/hooks';
import { setCurrentWeek, setCurrentQuarter } from '../store/slices/scheduleSlice';
import trpc from '../trpc';

export function useSetSchedule() {
  const dispatch = useAppDispatch();

  useEffect(() => {
    // set current week
    trpc.schedule.currentWeek.query().then((res) => {
      const week = res.display.split(' • ')[0];
      dispatch(setCurrentWeek(week));
    });

    // set current quarter
    trpc.schedule.currentQuarter.query().then((res) => {
      dispatch(setCurrentQuarter(res));
    });
  }, [dispatch]);
}
