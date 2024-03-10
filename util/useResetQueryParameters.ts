import { useRouter } from 'next/router';

export const useResetQueryParameters = (): (() => void) => {
  const router = useRouter();

  const resetQueryParameters = () => {
    const parametersToPreserve = ['userId', 'setSlug'];
    const query = Object.keys(router.query)
      .filter((key) => parametersToPreserve.includes(key))
      .reduce((obj, key) => {
        obj[key] = router.query[key];
        return obj;
      }, {});

    router
      .replace(
        {
          query,
        },
        undefined,
        { shallow: true }
      )
      .then(() => {
        window.scrollTo(0, 0);
      });
  };

  return resetQueryParameters;
};
