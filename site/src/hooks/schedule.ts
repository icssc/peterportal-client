import { useEffect } from 'react';
import { useAppDispatch, useAppSelector } from '../store/hooks';
import { fetchCurrentWeek, fetchCurrentQuarter } from '../store/slices/scheduleSlice';

export function useCurrentWeek() {
  const dispatch = useAppDispatch();
  const currentWeek = useAppSelector((state) => state.schedule.currentWeek);

  useEffect(() => {
    if (!currentWeek) {
      dispatch(fetchCurrentWeek());
    }
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
