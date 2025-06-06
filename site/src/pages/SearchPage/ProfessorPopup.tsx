import { FC, useEffect, useState } from 'react';
import SearchPopup from '../../component/SearchPopup/SearchPopup';

import { useAppSelector } from '../../store/hooks';
import { selectProfessor } from '../../store/slices/popupSlice';
import { ScoreData } from '../../types/types';
import trpc from '../../trpc';
import { FeaturedReviewData } from '@peterportal/types';

const ProfessorPopup: FC = () => {
  const professor = useAppSelector(selectProfessor);
  const [featured, setFeatured] = useState<FeaturedReviewData | undefined>();
  const [scores, setScores] = useState<ScoreData[]>([]);

  useEffect(() => {
    if (professor) {
      const reviewParams = {
        type: 'professor',
        id: professor.ucinetid,
      } as const;
      trpc.reviews.avgRating.query(reviewParams).then((res) => {
        const scores = res.map((course) => ({ name: course.name, avgRating: course.avgRating, id: course.name }));
        const scoredCourses = new Set(res.map((v) => v.name));
        Object.keys(professor.courses).forEach((course) => {
          // remove spaces
          course = course.replace(/\s+/g, '');
          // add unknown score
          if (!scoredCourses.has(course)) {
            scores.push({ name: course, avgRating: -1, id: course });
          }
        });
        // sort by highest score
        res.sort((a, b) => b.avgRating - a.avgRating);
        setScores(scores);
      });
      trpc.reviews.featured.query(reviewParams).then(setFeatured);
    }
  }, [professor]);

  if (!professor) {
    return <SearchPopup name="" id="" infos={[]} scores={[]} searchType="professor" title="" />;
  }

  // include basic info and featured review panels
  const infos = [
    {
      title: 'Basic Info',
      content: `Email: ${professor.ucinetid}@uci.edu`,
    },
    {
      title: `Featured Review`,
      content: featured
        ? `For ${featured.courseId}: ${(featured.content ?? '').length > 0 ? featured.content : 'Rating of ' + featured.rating + '/5'}`
        : 'No Reviews Yet!',
    },
  ];

  return (
    <SearchPopup
      name={professor.name}
      id={professor.ucinetid}
      title={professor.title}
      infos={infos}
      scores={scores}
      searchType="professor"
      professor={professor}
    />
  );
};

export default ProfessorPopup;
