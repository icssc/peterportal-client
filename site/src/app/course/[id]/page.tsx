import { FC } from 'react';
import CoursePage from '../../../pages/CoursePage';

interface CoursePageParams {
  params: { id: string };
}
const Page: FC<CoursePageParams> = ({ params }) => {
  return <CoursePage courseId={decodeURIComponent(params.id)} />;
};
export default Page;
