import { FC, useState, useEffect } from 'react';
import './index.scss';
import { useParams } from 'react-router-dom';

import SearchModule from '../../component/SearchModule/SearchModule';
import SearchHitContainer from '../../component/SearchHitContainer/SearchHitContainer';
import { SearchPopup, SearchPopupPlaceholder } from '../../component/SearchPopup/SearchPopup';

import { ScoreData, GQLData, SearchIndex, GQLDataType } from '../../types/types';
import { FeaturedReviewData } from '@peterportal/types';
import { useAppSelector } from '../../store/hooks';
import { capitalize, removeWhitespace } from '../../helpers/util';
import trpc from '../../trpc';

interface ResultPopupProps {
  dataType: GQLDataType;
}

const ResultPopup: FC<ResultPopupProps> = ({ dataType }) => {
  const { course, professor } = useAppSelector((state) => state.popup);
  const data: GQLData = dataType === 'course' ? course : professor;
  const [scores, setScores] = useState<ScoreData[]>([]);
  const [featured, setFeatured] = useState<FeaturedReviewData | undefined>();

  useEffect(() => {
    if (!data) return;

    const params = { type: data.type, id: data.type === 'course' ? data.id : data.ucinetid };

    if (data.type === 'professor') {
      trpc.reviews.featured.query(params).then(setFeatured);
    }

    trpc.reviews.avgRating.query(params).then((res) => {
      const scoredDataNames = new Set(res.map((v) => v.name));

      const dataList = data.type === 'course' ? data.instructors : data.courses;

      const scoresForRatedData: ScoreData[] = res
        .map((c) => ({
          name: data.type === 'professor' ? c.name : (data.instructors[c.name]?.name ?? c.name),
          avgRating: c.avgRating,
          id: c.name,
        }))
        .sort((a, b) => b.avgRating - a.avgRating);

      const scoresForUnratedData: ScoreData[] = Object.keys(dataList)
        .map(removeWhitespace)
        .filter((c) => !scoredDataNames.has(c))
        .map((c) => ({
          name: data.type === 'course' ? data.instructors[c].name : c,
          avgRating: -1,
          id: c,
        }));

      const scores = scoresForRatedData.concat(scoresForUnratedData);
      setScores(scores);
    });
  }, [data]);

  if (!data) {
    return <SearchPopupPlaceholder dataType={dataType} />;
  }

  const featuredReviewText = featured
    ? `For ${featured.courseId}: ${(featured?.content ?? '').length > 0 ? featured?.content : `Rating of ${featured?.rating}/5`}`
    : 'No Reviews Yet!';

  const infos =
    data.type === 'course'
      ? [
          { title: 'Prerequisite', content: data.prerequisiteText },
          { title: 'Restrictions', content: data.restriction },
        ]
      : [
          { title: 'Basic Info', content: `Email: ${data.ucinetid}@uci.edu` },
          { title: `Featured Review`, content: featuredReviewText },
        ];

  return (
    <SearchPopup
      data={data}
      id={data.type === 'course' ? data.id : data.ucinetid}
      name={data.type === 'course' ? data.id : data.name}
      title={data.title}
      infos={infos}
      scores={scores}
    />
  );
};

const SearchPage: FC = () => {
  const { index = 'courses' } = useParams<{ index: SearchIndex }>();

  useEffect(() => {
    document.title = `${capitalize(index)} | PeterPortal`;
  }, [index]);

  return (
    <div className="search-wrapper">
      <div className="search-list">
        <SearchModule index={index} />
        <SearchHitContainer index={index} />
      </div>
      <ResultPopup dataType={index.slice(0, -1) as GQLDataType} />
    </div>
  );
};

export default SearchPage;
