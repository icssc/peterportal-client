import { FC } from "react";
import { Pagination } from "react-bootstrap";
import { useAppDispatch, useAppSelector } from "src/store/hooks";
import { setPageNumber } from "src/store/slices/searchSlice";
import { SearchIndex } from "src/types/types";

interface SearchPaginationProps {
  index: SearchIndex;
}

// TODO: Make this a global constant (also in ../SearchModule/SearchModule.tsx)
const PAGE_SIZE = 10;

const SearchPagination: FC<SearchPaginationProps> = ({ index }) => {
  const dispatch = useAppDispatch();
  const coursePageNumber = useAppSelector(state => state.search.courses.pageNumber);
  const professorPageNumber = useAppSelector(state => state.search.professors.pageNumber);

  const courseData = useAppSelector(state => state.search.courses);
  const professorData = useAppSelector(state => state.search.professors);

  const clickPageNumber = (pageNumber: number) => {
    dispatch(setPageNumber({index, pageNumber}));
  }

  const maxPageNumber = index === 'courses' ? 
    Math.ceil(courseData.names.length / PAGE_SIZE) : 
    Math.ceil(professorData.names.length / PAGE_SIZE);
  const active = index === 'courses' ? courseData.pageNumber : courseData.pageNumber;
  let items = [];
  for (let i = 0; i < maxPageNumber; i++) {
    items.push(
      <Pagination.Item key={i} active={i === active} onClick={() => clickPageNumber(i)}>
        {i + 1}
      </Pagination.Item>,
    );
  }

  return <Pagination size="lg">{items}</Pagination>
}

export default SearchPagination;