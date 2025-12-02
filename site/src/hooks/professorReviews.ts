import { useEffect, useState } from 'react';
import { useAppDispatch, useAppSelector } from '../store/hooks';
import trpc from '../trpc';
import { ProfessorGQLData } from '../types/types';
import { transformProfessorGQL } from '../helpers/util.tsx';
import { setProfessor } from '../store/slices/professorReviewsSlice.ts';

// Get a professor's info
// If it is not in cache then get from API and put in cache

export function useProfessorData(netID: string) {
  const professorCache = useAppSelector((state) => state.professors.professors);
  const [loadTrigger, setLoadTrigger] = useState(false);
  const [fullProfessorData, setFullProfessorData] = useState<ProfessorGQLData | null>(professorCache[netID] ?? null);

  const dispatch = useAppDispatch();

  useEffect(() => {
    // Use a stateful trigger to avoid sending two requests as a result of double first render
    setLoadTrigger(true);
  }, [netID]);

  useEffect(() => {
    if (!loadTrigger) return;
    setLoadTrigger(false);

    const cachedProfessor = professorCache[netID];

    if (cachedProfessor) {
      setFullProfessorData(cachedProfessor);
      return;
    }
    trpc.professors.get.query({ ucinetid: netID }).then((professor) => {
      const transformedProfessor = transformProfessorGQL(professor);
      setFullProfessorData(transformedProfessor);

      dispatch(setProfessor({ professorId: netID, data: transformedProfessor }));
    });
  }, [netID, dispatch, loadTrigger, professorCache]);

  return fullProfessorData;
}
