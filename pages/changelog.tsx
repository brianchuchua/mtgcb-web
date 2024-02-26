import Typography from '@material-ui/core/Typography';
import { ResponsiveContainer } from '../components/layout/ResponsiveContainer';

const ChangelogPage: React.FC = () => (
  <ResponsiveContainer maxWidth="xl">
    <Typography variant="h3" component="h1" align="center">
      Changelog
    </Typography>
    <Typography variant="h5" component="h2">
      2024-02-25 (v0.57.1)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Fixed a major bug that was failing to filter by exact card type match. (Thanks Gluthoric for the report!)</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2023-12-23 (v0.57.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Added new subset dropdown for subset groups.</li>
        <li>Split Secret Lair Drop Series into hundreds of subsets.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2023-12-01 (v0.56.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Updated to TCGPlayer's new linking structure.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2023-09-02 (v0.55.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Aside from lots of database work, added confetti when hitting 100% completion in a set.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2023-06-26 (v0.54.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Added 12 and 24 as options for number of cards per page.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2023-05-17 (v0.53.3)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Fixed a bug that excluded cards from search results when they were being sorted by properties they lacked.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2023-02-01 (v0.53.2)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Ensured that subset groups render the proper number of cards if a subset is a child of it.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2023-01-26 (v0.53.1)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Fixed a rare bug where server calls would sometimes fail due to bad subset values.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2023-01-26 (v0.53.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Improved error monitoring tools.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2023-01-26 (v0.52.1)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Fixed a bug with set card counts in set table views.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2023-01-23 (v0.52.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Added support for subsets and subset groups.</li>
        <li>Did a proof-of-concept migration of Tempest Prerelease Promos into their own subset. (Many more migrations yet to do!)</li>
        <li>Fixed a bug with resetting text form fields.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2023-01-16 (v0.51.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Added compact set view mode.</li>
        <li>Fixed bugs with rendering percentage completion and card price updates.</li>
        <li>Made minor performance enhancements when rendering a large number of items.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2023-01-11 (v0.50.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Added easier quantity selector buttons.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2023-01-11 (v0.49.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Added ability to search sets by set code.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2023-01-10 (v0.48.1)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Fixed performance issues with searching by card name, set name, oracle text, or artist.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2023-01-10 (v0.48.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Added ability to search by artist name.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2023-01-09 (v0.47.1)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Added MTG CB Collector Numbers and fixed bugs with collection number sorting.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-12-29 (v0.47.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Added MTG CB Collector Numbers and fixed bugs with collection number sorting.</li>
        <li>Added ability to sort even older sets by collector number!</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-12-20 (v0.46.3)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Officially switched over to the new backend API and database.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-12-14 (v0.46.2)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Fixed a bug with fetching collection details filtered by collected amounts.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-12-12 (v0.46.1)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Make success pop-up notification locations consistent.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-12-11 (v0.46.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Other than adding thousands of new cards:</li>
        <li>Completely rewrote and migrated to a new backend API and database.</li>
        <li>Changed logging in to redirect the user to the Collection page afterwards.</li>
        <li>Fixed a bug where sets weren't being filtered by completion status in table view.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-09-08 (v0.45.3)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Allowed Edit Cards page to render 500 search results (instead of 50).</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-08-10 (v0.45.2)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Fixed issue with image warping when using the cards per row slider via clicking.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-08-06 (v0.45.1)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Fix bugs with performing searches mid-pagination.</li>
        <li>Improve loading state for slow pagination (collections filtered by quantity)</li>
        <li>Improve image loading animations.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-07-22 (v0.45.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Added deep linking of searches -- you can share any search with friends by copying the URL! It now updates as you search.</li>
        <li>Added a reset search button to the bottom of each search form.</li>
        <li>Made view subject, view mode, and price preference sticky per user device.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-07-19 (v0.44.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>
          Made a bunch of settings saved to your device -- number of cards per row, card sizes, cards per page, sets per page, and more!
        </li>
        <li>Changed the default number of cards from 4 to 5 -- it looked too zoomed in.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-07-16 (v0.43.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Added pagination to bottom of pages (along with a "back to top" button.)</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-07-14 (v0.42.1)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Fixed a bug that caused the card gallery to fully reset when a card quantity was updated.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-07-12 (v0.42.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>
          Added filters and sorting for cards at the root-level collection page. (These were previously only available within a set page.)
        </li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-07-10 (v0.41.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Added louder success pop-up when a card is updated outside the Edit Cards page.</li>
        <li>Added error pop-ups if card updating fails due to an API outage.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-07-04 (v0.40.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Improved loading states / added image placeholders / reduced layout shift.</li>
        <li>Added prefetching logic so the next page in pagination is fetched in advance.</li>
        <li>Made set icons clickable to enter set pages.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-06-22 (v0.39.4)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Improved caching behavior for collection updates.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-06-21 (v0.39.3)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Fixed broken Buy links on mobile Safari.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-06-20 (v0.39.2)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Fixed scrolling bug on MacOS Safari.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-06-19 (v0.39.1)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Added expansion-level filters and sorting for cards.</li>
        <li>New filtering options:</li>
        <ul>
          <li>Quantity (All) - Foils and non-foils between any quantities you set.</li>
          <li>Quantity (Normal) - Non-foils between any quantities you set.</li>
          <li>Quantity (Foil) - Traditional foils in any quantities you set.</li>
        </ul>
        <li>New sorting options:</li>
        <ul>
          <li>Quantity (All)</li>
          <li>Quantity (Normal)</li>
          <li>Quantity (Foil)</li>
        </ul>
        <li>Fixed a bug where expansion-level searches were broken.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-05-30 (v0.38.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Added collection set filters and sorting.</li>
        <li>New filtering options:</li>
        <ul>
          <li>All - Show all sets.</li>
          <li>Complete - Show sets at 100% completion.</li>
          <li>Partial - Show sets between 1% and 99% completion.</li>
          <li>Empty - Show sets at 0% completion.</li>
        </ul>
        <li>New sorting options:</li>
        <ul>
          <li>Current Value - Sort by current value.</li>
          <li>Cost to Complete - Sort by cost to complete.</li>
          <li>% Collected - Sort by % collected.</li>
        </ul>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-04-10 (v0.37.3)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Full mobile optimization pass for every site component.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-04-04 (v0.37.2)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Fixed a bug with case-sensivity during account migration.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-04-03 (v0.37.1)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Changed some components to use native versions on mobile! (More of these to come.)</li>
        <li>Made the MTG CB beta public!</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-03-10 (v0.36.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Added Edit Cards page!</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-03-03 (v0.35.1)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Fixed card quantities being overriden during parallel updates.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-02-20 (v0.35.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Added ability to update collection in set and collection views.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-01-11 (v0.34.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Allowed mobile sidenav to close when the user presses the region outside of it.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-01-09 (v0.33.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Made the site usable on mobile devices with a dozen related UI changes.</li>
        <li>Reverted the browse view to default to viewing sets first.</li>
        <li>Removed breadcrumbs from the top of pages -- they were just taking up space redundantly.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-01-03 (v0.32.2)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>
          Fixed a bug causing blank search results due to filters changing after the user navigated to a further page of a previous search.
          The page will properly reset back to 1 now.
        </li>
        <li>Fixed a bug where the Collection menu item was not always active when viewing your collection.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-01-02 (v0.32.1)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>
          Fixed several client-side errors reported in Sentry.io -- viewing a table "too quickly" would cause a crash if data weren't
          available for it yet.
        </li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-01-01 (v0.32.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Fixed a bug where unique cards completed weren't being calculated properly on the Set collection view.</li>
        <li>Made it clearer which pages are still not yet implemented.</li>
        <li>Corrected percentage rounding issue. (281/282 cards is 99%, not 100%.)</li>
        <li>Fixed the login issue on mobile devices / WebKit browsers.</li>
        <li>Changed the site and API to live on the mtgcollectionbuilder.com domain.</li>
        <li>Set up a redirect from the old domain to the new domain.</li>
      </ul>
    </Typography>
  </ResponsiveContainer>
);

export default ChangelogPage;
