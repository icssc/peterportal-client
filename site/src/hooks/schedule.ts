import { useEffect } from 'react';
import { useAppDispatch, useAppSelector } from '../store/hooks';
import { fetchCurrentWeek } from '../store/slices/scheduleSlice';

export function useCurrentWeek() {
  const dispatch = useAppDispatch();
  const week = useAppSelector((state) => state.schedule.currentWeek);

  useEffect(() => {
    if (!week) {
      dispatch(fetchCurrentWeek());
    }
  }, [week, dispatch]);

  return { week };
}
