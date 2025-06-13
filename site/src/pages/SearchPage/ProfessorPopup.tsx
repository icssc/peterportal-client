import { FC, useState, useEffect } from 'react';
import { SearchPopup, SearchPopupPlaceholder } from '../../component/SearchPopup/SearchPopup';
import { useAppSelector } from '../../store/hooks';
import { selectProfessor } from '../../store/slices/popupSlice';
import { removeWhitespace } from '../../helpers/util';
import { ScoreData } from '../../types/types';
import { FeaturedReviewData } from '@peterportal/types';
import trpc from '../../trpc';

const ProfessorPopup: FC = () => {
  const professor = useAppSelector(selectProfessor);
  const [featured, setFeatured] = useState<FeaturedReviewData | undefined>();
  const [scores, setScores] = useState<ScoreData[]>([]);

  useEffect(() => {
    if (!professor) return;

    const reviewParams = {
      type: 'professor',
      id: professor.ucinetid,
    } as const;

    trpc.reviews.avgRating.query(reviewParams).then((res) => {
      const scores = res.map((course) => ({ name: course.name, avgRating: course.avgRating, id: course.name }));
      const scoredCourses = new Set(res.map((v) => v.name));
      Object.keys(professor.courses).forEach((course) => {
        // remove spaces
        course = removeWhitespace(course);
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
  }, [professor]);

  if (!professor) {
    return <SearchPopupPlaceholder dataType="professor" />;
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
      dataType="professor"
      data={professor}
      id={professor.ucinetid}
      name={professor.name}
      title={professor.title}
      infos={infos}
      scores={scores}
    />
  );
};

export default ProfessorPopup;
