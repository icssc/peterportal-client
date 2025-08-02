import CoursePage from '../../../pages/CoursePage';

interface CoursePageParams {
  params: Promise<{ id: string }>;
}
const Page = async ({ params }: CoursePageParams) => {
  return <CoursePage courseId={decodeURIComponent((await params).id)} />;
};
export default Page;
