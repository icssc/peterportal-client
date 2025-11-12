import CoursePage from '../CoursePage';

export async function generateMetadata({ params }: { params: Promise<{ id: string }> }) {
  const id = decodeURIComponent((await params).id);
  return {
    title: `${id} | PeterPortal`,
    description: `View recent offerings, prerequisites, grade distributions, and reviews for ${id} at UC Irvine.`,
  };
}

interface CoursePageParams {
  params: Promise<{ id: string }>;
}
const Page = async ({ params }: CoursePageParams) => {
  return <CoursePage courseId={decodeURIComponent((await params).id)} />;
};
export default Page;
