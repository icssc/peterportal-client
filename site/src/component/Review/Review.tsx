import { FC, useState, useEffect, useCallback, useContext } from 'react';
import './Review.scss';
import { Button, Dropdown, DropdownButton, Form } from 'react-bootstrap';

import SubReview from './SubReview';
import ReviewForm from '../ReviewForm/ReviewForm';
import LoadingSpinner from '../LoadingSpinner/LoadingSpinner';

import { selectReviews, setReviews, setFormStatus } from '../../store/slices/reviewSlice';
import { useAppSelector, useAppDispatch } from '../../store/hooks';
import { CourseGQLData, ProfessorGQLData, GQLData } from '../../types/types';
import { ReviewData } from '@peterportal/types';
import trpc from '../../trpc';
import ThemeContext from '../../style/theme-context';

import AddIcon from '@mui/icons-material/Add';

interface Option {
  text: string;
  value: string | number;
}

enum SortingOption {
  MOST_RECENT,
  TOP_REVIEWS,
  CONTROVERSIAL,
}

interface ReviewProps {
  data: GQLData;
}

interface SortFilterMenuProps {
  data: GQLData;
  sortedReviews: ReviewData[];
  sortingOption: SortingOption;
  setSortingOption: (option: SortingOption) => void;
  filterOption: string;
  setFilterOption: (option: string) => void;
  showOnlyVerifiedReviews: boolean;
  setShowOnlyVerifiedReviews: (showOnly: boolean) => void;
}

const SortFilterMenu: FC<SortFilterMenuProps> = ({
  data,
  sortedReviews,
  sortingOption,
  setSortingOption,
  filterOption,
  setFilterOption,
  showOnlyVerifiedReviews,
  setShowOnlyVerifiedReviews,
}) => {
  const { darkMode } = useContext(ThemeContext);
  const buttonVariant = darkMode ? 'dark' : 'light';

  const reviewSortOptions = [
    { text: 'Most Recent', value: SortingOption.MOST_RECENT },
    { text: 'Top Reviews', value: SortingOption.TOP_REVIEWS },
    { text: 'Controversial', value: SortingOption.CONTROVERSIAL },
  ];

  const selectedSortOptionText = reviewSortOptions.find((x) => x.value === sortingOption)?.text ?? 'Sort Reviews...';

  // calculate frequencies of professors or courses in list of reviews
  const reviewFreq = sortedReviews.reduce((acc, review) => {
    const key = data.type === 'course' ? review.professorId : review.courseId;
    return acc.set(key, (acc.get(key) || 0) + 1);
  }, new Map<string, number>());

  const dataOptions: Option[] = [
    {
      text: data.type === 'course' ? 'All Professors' : 'All Courses',
      value: '',
    },
    ...Object.keys(data.type === 'course' ? (data as CourseGQLData).instructors : (data as ProfessorGQLData).courses)
      .map((id) => ({
        text: `${
          data.type === 'course'
            ? ((data as CourseGQLData).instructors[id]?.name ?? id)
            : `${(data as ProfessorGQLData).courses[id]?.department} ${(data as ProfessorGQLData).courses[id]?.courseNumber}`
        } (${reviewFreq.get(id) || 0})`,
        value: id,
      }))
      .filter(({ value }) => reviewFreq.get(value))
      .sort((a, b) => a.text.localeCompare(b.text)),
  ];

  const selectedOptionText =
    dataOptions.find((option) => option.value === filterOption)?.text ??
    `Select ${data.type === 'course' ? 'Professor' : 'Course'}...`;

  interface TempButtonProps {
    title: string;
    onSelect?: (eventKey: string | null) => void;
    options: Option[];
  }

  const SortDropdownButton: FC<TempButtonProps> = ({ title, onSelect, options }) => {
    return (
      <div className="sort-dropdown">
        <DropdownButton className="ppc-dropdown-btn" title={title} variant={buttonVariant} onSelect={onSelect}>
          {options.map((option) => (
            <Dropdown.Item key={option.value} eventKey={option.value}>
              {option.text}
            </Dropdown.Item>
          ))}
        </DropdownButton>
      </div>
    );
  };

  return (
    <div className="sort-filter-menu">
      <SortDropdownButton
        title={selectedSortOptionText}
        onSelect={(value) => setSortingOption(Number(value) as SortingOption)}
        options={reviewSortOptions}
      />
      <SortDropdownButton
        title={selectedOptionText}
        onSelect={(value) => setFilterOption(value as string)}
        options={dataOptions}
      />
      <div className="verified-only-checkbox">
        <Form>
          <Form.Check
            type="checkbox"
            label="Show verified reviews only"
            id="Show verified reviews only"
            checked={showOnlyVerifiedReviews}
            onChange={() => setShowOnlyVerifiedReviews(!showOnlyVerifiedReviews)}
          />
        </Form>
      </div>
    </div>
  );
};

const Review: FC<ReviewProps> = ({ data }) => {
  const dispatch = useAppDispatch();
  const reviewData = useAppSelector(selectReviews);
  const showForm = useAppSelector((state) => state.review.formOpen);
  const [sortingOption, setSortingOption] = useState<SortingOption>(SortingOption.MOST_RECENT);
  const [filterOption, setFilterOption] = useState('');
  const [showOnlyVerifiedReviews, setShowOnlyVerifiedReviews] = useState(false);

  const getReviews = useCallback(async () => {
    // prevent reviews from carrying over
    dispatch(setReviews([]));
    const params =
      data.type === 'course'
        ? { courseId: (data as CourseGQLData).id }
        : { professorId: (data as ProfessorGQLData).ucinetid };
    const reviews = await trpc.reviews.get.query(params);
    dispatch(setReviews(reviews));
  }, [dispatch, data]);

  useEffect(() => {
    getReviews();
  }, [getReviews]);

  if (!reviewData) {
    return <LoadingSpinner />;
  }

  const sortedReviews = [...reviewData]
    .filter((review) => !showOnlyVerifiedReviews || review.verified)
    .filter(
      (review) =>
        filterOption.length === 0 || filterOption === (data.type === 'course' ? review.professorId : review.courseId),
    )
    .sort((a, b) => {
      const mostRecentSort = () => new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime();
      switch (sortingOption) {
        case SortingOption.TOP_REVIEWS:
          return b.score - a.score || mostRecentSort();
        case SortingOption.CONTROVERSIAL:
          return a.score - b.score || mostRecentSort();
        case SortingOption.MOST_RECENT:
        default:
          return mostRecentSort();
      }
    });

  const openReviewForm = () => {
    dispatch(setFormStatus(true));
    document.body.style.overflow = 'hidden';
  };

  const closeForm = () => {
    dispatch(setFormStatus(false));
    document.body.style.overflow = 'visible';
  };

  return (
    <>
      <div className="reviews">
        <SortFilterMenu
          data={data}
          sortedReviews={sortedReviews}
          sortingOption={sortingOption}
          setSortingOption={setSortingOption}
          filterOption={filterOption}
          setFilterOption={setFilterOption}
          showOnlyVerifiedReviews={showOnlyVerifiedReviews}
          setShowOnlyVerifiedReviews={setShowOnlyVerifiedReviews}
        />
        <div className="subreviews">
          {sortedReviews.map((review) => (
            <SubReview review={review} key={review.id} dataType={data.type} data={data} />
          ))}
        </div>
        <Button variant="primary" className="add-review-button" onClick={openReviewForm}>
          <AddIcon /> Add Review
        </Button>
      </div>
      <ReviewForm closeForm={closeForm} show={showForm} dataType={data.type} data={data} />
    </>
  );
};

export default Review;
