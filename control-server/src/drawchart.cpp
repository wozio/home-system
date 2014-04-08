#include "drawchart.h"

#include "gd.h"
#include "gdfontl.h"

#include <yami4-cpp/yami.h>

#include <boost/date_time/local_time_adjustor.hpp>
#include <boost/date_time/c_local_time_adjustor.hpp>

using namespace boost::filesystem;
using namespace boost::posix_time;
using namespace std;

#ifdef _MSC_VER
double round(double r, double precision = 0.0)
{
  double sd;
  if (precision == 0)
  {
    sd = 1;
  }
  else
  {
    sd = pow(10, precision);
  }
  return int(r * sd + (r < 0 ? -0.5 : 0.5)) / sd;
}
#endif

double round_up(double r, double precision = 0.0)
{
  double sd;
  if (precision == 0)
  {
    sd = 1;
  }
  else
  {
    sd = pow(10, precision);
  }
  return int(r * sd + 0.5) / sd;
}

double round_down(double r, double precision = 0.0)
{
  double sd;
  if (precision == 0)
  {
    sd = 1;
  }
  else
  {
    sd = pow(10, precision);
  }
  return int(r * sd) / sd;
}

path DrawTempChart(const string& server_address, const ptime& time_begin, const ptime& time_end, const vector<string>& sensors)
{
  yami::agent client_agent;
  yami::parameters params;

  tm tb = to_tm(time_begin);
  tm te = to_tm(time_end);
  params.set_integer("time_begin", mktime(&tb));
  params.set_integer("time_end", mktime(&te));

  yami::outgoing_message msg;

  client_agent.send(msg, "tcp://" + server_address + ":9998", "sensors", "get_sensor_data", params);
  msg.wait_for_completion(5000);

  if (msg.get_state() == yami::replied)
  {
    const yami::parameters& reply = msg.get_reply();
    size_t s;
    int* times = reply.get_integer_array("time", s);
    double* temps = reply.get_double_float_array("temp", s);

    // finding minimum and maximum temperature with times
    double min = 200, max = -200;
    int min_time, max_time;
    for (size_t i = 0; i < s; ++i)
    {
      if (temps[i] > max)
      {
        max = temps[i];
        max_time = times[i];
      }
      if (temps[i] < min)
      {
        min = temps[i];
        min_time = times[i];
      }
    }

    // chart properties
    size_t img_x = 1200;
    size_t img_y = 800;
    size_t left_margin = 50;
    size_t right_margin = 50;
    size_t bottom_margin = 50;
    size_t top_margin = 50;
    size_t chart_x = img_x - left_margin - right_margin;
    size_t chart_y = img_y - top_margin - bottom_margin;

    // initializing gd
    gdImagePtr im = gdImageCreate(img_x, img_y);
    gdImageColorAllocate(im, 255, 255, 255);
    int black = gdImageColorAllocate(im, 0, 0, 0);
    int red = gdImageColorAllocate(im, 255, 50, 50);

    // min max
    // TODO to be removed
    typedef boost::date_time::c_local_adjustor<ptime> local_adj;
    {
      ostringstream s;
      s << "minimum: " << setiosflags(ios::fixed) << setprecision(2) << min << " at " << local_adj::utc_to_local(from_time_t(min_time));
      gdImageString(im, gdFontGetLarge(), 0, 0, (unsigned char *)s.str().c_str(), black);
    }
    {
      ostringstream s;
      s << "maximum: " << setiosflags(ios::fixed) << setprecision(2) << max << " at " << local_adj::utc_to_local(from_time_t(max_time));
      gdImageString(im, gdFontGetLarge(), 0, gdFontGetLarge()->h + 2, (unsigned char *)s.str().c_str(), black);
    }

    // axes
    gdImageLine(im, left_margin, top_margin, left_margin, top_margin + chart_y, black);
    gdImageLine(im, left_margin + chart_x, top_margin, left_margin + chart_x, top_margin + chart_y, black);
    gdImageLine(im, left_margin, top_margin + chart_y, left_margin + chart_x, top_margin + chart_y, black);
    gdImageLine(im, left_margin, top_margin, left_margin + chart_x, top_margin, black);

    // grid lines and labels on y axis
    double y_grid_step = (max - min) / 40;
    double y_grid_pos;
    if (y_grid_step < 0.1)
    {
      y_grid_step = 0.1;
      y_grid_pos = round_up(min, 1);
    }
    else if (y_grid_step < 0.5)
    {
      y_grid_step = 0.25;
      y_grid_pos = round_up(min);
      if (y_grid_pos - min > 0.25)
      {
        y_grid_pos += 0.25;
      }
    }
    else if (y_grid_step < 1.0)
    {
      y_grid_step = 0.5;
      y_grid_pos = round_up(min);
      if (y_grid_pos - min > 0.5)
      {
        y_grid_pos += 0.5;
      }
    }
    else
    {
      y_grid_step = round(y_grid_step);
      y_grid_pos = round_up(min);
    }

    size_t zero_y = top_margin + chart_y;
    double y_scale = chart_y / (max - min);
    size_t x_pos = left_margin - 3;
    size_t x_pos_end = left_margin + chart_x;
    while (y_grid_pos < max)
    {
      size_t y_pos = zero_y - static_cast<size_t>((y_grid_pos - min) * y_scale);
      gdImageLine(im, x_pos, y_pos, x_pos_end, y_pos, black);
      ostringstream s;
      s << setiosflags(ios::fixed) << setprecision(2) << y_grid_pos;
      gdImageString(im, gdFontGetLarge(), x_pos - s.str().size() * gdFontGetLarge()->w, y_pos - gdFontGetLarge()->h / 2,
                    (unsigned char *)s.str().c_str(), black);
      y_grid_pos += y_grid_step;
    }

    // grid lines and labels on x axis
    int period_seconds = time_period(time_begin, time_end).length().total_seconds();
    double x_scale = (double)chart_x / (double)period_seconds;
    ptime t = time_end;
    size_t y_pos = chart_y + top_margin + 3;
    x_pos = left_margin + static_cast<size_t>(time_period(time_begin, t).length().total_seconds() * x_scale);

    // printing date element here and calculating variables for use in loop
    ostringstream s_date;
    time_facet* facet_date(new time_facet("%y-%m-%d"));
    s_date.imbue(std::locale(std::cout.getloc(), facet_date));
    s_date<< t;
    size_t date_string_width = s_date.str().size() * gdFontGetLarge()->w;
    size_t string_middle1 = date_string_width / 2;
    gdImageString(im, gdFontGetLarge(), x_pos - string_middle1, y_pos, (unsigned char *)s_date.str().c_str(), black);

    // width of the date string limit maximum number of labels on x axis
    // so calculate time step
    time_duration time_step(seconds(period_seconds / (chart_x / (date_string_width + 6))));

    ostringstream s_time;
    time_facet* facet_time(new time_facet("%H:%M:%S"));
    s_time.imbue(std::locale(std::cout.getloc(), facet_time));
    s_time << t;
    size_t string_middle2 = s_time.str().size() * gdFontGetLarge()->w / 2;
    size_t y_pos2 = y_pos + 3 + gdFontGetLarge()->h;
    gdImageString(im, gdFontGetLarge(), x_pos - string_middle2, y_pos2, (unsigned char *)s_time.str().c_str(), black);

    t -= time_step;

    while (t > time_begin)
    {
      x_pos = left_margin + static_cast<size_t>(time_period(time_begin, t).length().total_seconds() * x_scale);
      gdImageLine(im, x_pos, top_margin, x_pos, y_pos, black);

      s_date.str("");
      s_date << t;
      gdImageString(im, gdFontGetLarge(), x_pos - string_middle1, y_pos, (unsigned char *)s_date.str().c_str(), black);

      s_time.str("");
      s_time << t;
      gdImageString(im, gdFontGetLarge(), x_pos - string_middle2, y_pos2, (unsigned char *)s_time.str().c_str(), black);
      t -= time_step;
    }

    // chart
    size_t data_step;
    if (chart_x < s)
    {
      data_step = s / chart_x;
    }
    else
    {
      data_step = 1;
    }

    //typedef boost::date_time::c_local_adjustor<ptime> local_adj;
    x_pos = left_margin + static_cast<size_t>(time_period(time_begin, local_adj::utc_to_local(from_time_t(times[0]))).length().total_seconds() * x_scale);
    y_pos = zero_y - static_cast<size_t>((temps[0] - min) * y_scale);
    for (size_t i = 1; i < s; i += data_step)
    {
      size_t new_y_pos = zero_y - static_cast<size_t>((temps[i] - min) * y_scale);
      size_t new_x_pos = left_margin + static_cast<size_t>(time_period(time_begin, local_adj::utc_to_local(from_time_t(times[i]))).length().total_seconds() * x_scale);
      gdImageLine(im, x_pos, y_pos, new_x_pos, new_y_pos, red);
      y_pos = new_y_pos;
      x_pos = new_x_pos;
    }

    FILE *pngout = fopen("test.png", "wb");
    gdImagePng(im, pngout);
    fclose(pngout);
    gdImageDestroy(im);
  }
  else
  {
    cout << "jakis blad!" << endl;
  }
  path p;
  return p;
}
